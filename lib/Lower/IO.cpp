//===-- IO.cpp -- I/O statement lowering ----------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "flang/Lower/IO.h"
#include "../../runtime/io-api.h"
#include "RTBuilder.h"
#include "flang/Lower/Bridge.h"
#include "flang/Lower/OpBuilder.h"
#include "flang/Lower/Runtime.h"
#include "flang/Optimizer/Dialect/FIROps.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Semantics/tools.h"
#include "mlir/Dialect/StandardOps/IR/Ops.h"
#include "mlir/IR/Builders.h"

#define TODO() assert(false && "not yet implemented")

using namespace Fortran::runtime::io;

namespace Fortran::lower {
namespace {

#define NAMIFY_HELPER(X) #X
#define NAMIFY(X) NAMIFY_HELPER(IONAME(X))
#define mkIOKey(X) mkKey(IONAME(X))

/// Static table of IO runtime calls
///
/// This logical map contains the name and type builder function for each IO
/// runtime function listed in the tuple. This table is fully constructed at
/// compile-time. Use the `mkIOKey` macro to access the table.
static constexpr std::tuple<
    mkIOKey(BeginInternalArrayListOutput), mkIOKey(BeginInternalArrayListInput),
    mkIOKey(BeginInternalArrayFormattedOutput),
    mkIOKey(BeginInternalArrayFormattedInput), mkIOKey(BeginInternalListOutput),
    mkIOKey(BeginInternalListInput), mkIOKey(BeginInternalFormattedOutput),
    mkIOKey(BeginInternalFormattedInput), mkIOKey(BeginInternalNamelistOutput),
    mkIOKey(BeginInternalNamelistInput), mkIOKey(BeginExternalListOutput),
    mkIOKey(BeginExternalListInput), mkIOKey(BeginExternalFormattedOutput),
    mkIOKey(BeginExternalFormattedInput), mkIOKey(BeginUnformattedOutput),
    mkIOKey(BeginUnformattedInput), mkIOKey(BeginExternalNamelistOutput),
    mkIOKey(BeginExternalNamelistInput), mkIOKey(BeginAsynchronousOutput),
    mkIOKey(BeginAsynchronousInput), mkIOKey(BeginWait), mkIOKey(BeginWaitAll),
    mkIOKey(BeginClose), mkIOKey(BeginFlush), mkIOKey(BeginBackspace),
    mkIOKey(BeginEndfile), mkIOKey(BeginRewind), mkIOKey(BeginOpenUnit),
    mkIOKey(BeginOpenNewUnit), mkIOKey(BeginInquireUnit),
    mkIOKey(BeginInquireFile), mkIOKey(BeginInquireIoLength),
    mkIOKey(EnableHandlers), mkIOKey(SetAdvance), mkIOKey(SetBlank),
    mkIOKey(SetDecimal), mkIOKey(SetDelim), mkIOKey(SetPad), mkIOKey(SetPos),
    mkIOKey(SetRec), mkIOKey(SetRound), mkIOKey(SetSign),
    mkIOKey(OutputDescriptor), mkIOKey(InputDescriptor),
    mkIOKey(OutputUnformattedBlock), mkIOKey(InputUnformattedBlock),
    mkIOKey(OutputInteger64), mkIOKey(InputInteger64), mkIOKey(OutputReal32),
    mkIOKey(InputReal32), mkIOKey(OutputReal64), mkIOKey(InputReal64),
    mkIOKey(OutputComplex64), mkIOKey(OutputComplex32), mkIOKey(OutputAscii),
    mkIOKey(InputAscii), mkIOKey(OutputLogical), mkIOKey(InputLogical),
    mkIOKey(SetAccess), mkIOKey(SetAction), mkIOKey(SetAsynchronous),
    mkIOKey(SetEncoding), mkIOKey(SetEncoding), mkIOKey(SetForm),
    mkIOKey(SetPosition), mkIOKey(SetRecl), mkIOKey(SetStatus),
    mkIOKey(SetFile), mkIOKey(GetNewUnit), mkIOKey(GetSize),
    mkIOKey(GetIoLength), mkIOKey(GetIoMsg), mkIOKey(InquireCharacter),
    mkIOKey(InquireLogical), mkIOKey(InquirePendingId),
    mkIOKey(InquireInteger64), mkIOKey(EndIoStatement)>
    newIOTable;

/// Helper function to retrieve the name of the IO function given the key `A`
template <typename A>
static constexpr const char *getName() {
  return std::get<A>(newIOTable).name;
}

/// Helper function to retrieve the type model signature builder of the IO
/// function as defined by the key `A`
template <typename A>
static constexpr FuncTypeBuilderFunc getTypeModel() {
  return std::get<A>(newIOTable).getTypeModel();
}

inline bool isCharacterLiteral(mlir::Type argTy) {
  if (auto arrTy = argTy.dyn_cast<fir::SequenceType>())
    return arrTy.getEleTy().isa<fir::CharacterType>();
  return false;
}

inline int64_t getLength(mlir::Type argTy) {
  return argTy.cast<fir::SequenceType>().getShape()[0];
}

/// Get (or generate) the MLIR FuncOp for a given IO runtime function.
template <typename E>
mlir::FuncOp getIORuntimeFunc(mlir::OpBuilder &builder) {
  mlir::ModuleOp module = getModule(&builder);
  auto name = getName<E>();
  auto func = getNamedFunction(module, name);
  if (func)
    return func;
  auto funTy = getTypeModel<E>()(builder.getContext());
  func = createFunction(module, name, funTy);
  func.setAttr("fir.runtime", builder.getUnitAttr());
  func.setAttr("fir.io", builder.getUnitAttr());
  return func;
}

/// Generate a call to end an IO statement
mlir::Value genEndIO(mlir::OpBuilder &builder, mlir::Location loc,
                     mlir::Value cookie) {
  // Terminate IO
  auto endIOFunc = getIORuntimeFunc<mkIOKey(EndIoStatement)>(builder);
  llvm::SmallVector<mlir::Value, 1> endArgs{cookie};
  auto call = builder.create<mlir::CallOp>(loc, endIOFunc, endArgs);
  return call.getResult(0);
}

using FormatItems = std::optional<std::pair<mlir::Value, mlir::Value>>;

/// Translate a list of format-items into a set of call-backs that can be
/// emitted into the MLIR stream before each data item is processed
FormatItems lowerFormat(AbstractConverter &converter,
                        const Fortran::parser::Format &format) {
  FormatItems formatItems;
  std::visit(Fortran::common::visitors{
                 [](const Fortran::parser::DefaultCharExpr
                        &) { /* string expression */ },
                 [](const Fortran::parser::Label &) { /* FORMAT statement */ },
                 [](const Fortran::parser::Star &) {},
             },
             format.u);
  return formatItems;
}

mlir::FuncOp getOutputRuntimeFunc(mlir::OpBuilder &builder, mlir::Type type) {
  if (auto ty = type.dyn_cast<mlir::IntegerType>()) {
    if (ty.getWidth() == 1)
      return getIORuntimeFunc<mkIOKey(OutputLogical)>(builder);
    return getIORuntimeFunc<mkIOKey(OutputInteger64)>(builder);
  } else if (auto ty = type.dyn_cast<mlir::FloatType>()) {
    if (ty.getWidth() <= 32)
      return getIORuntimeFunc<mkIOKey(OutputReal32)>(builder);
    return getIORuntimeFunc<mkIOKey(OutputReal64)>(builder);
  } else if (auto ty = type.dyn_cast<fir::CplxType>()) {
    if (ty.getFKind() <= 4)
      return getIORuntimeFunc<mkIOKey(OutputComplex32)>(builder);
    return getIORuntimeFunc<mkIOKey(OutputComplex64)>(builder);
  } else if (auto ty = type.dyn_cast<fir::LogicalType>()) {
    return getIORuntimeFunc<mkIOKey(OutputLogical)>(builder);
  } else if (auto ty = type.dyn_cast<fir::BoxType>()) {
    return getIORuntimeFunc<mkIOKey(OutputDescriptor)>(builder);
  } else {
    return getIORuntimeFunc<mkIOKey(OutputAscii)>(builder);
  }
}

/// The I/O library interface requires that COMPLEX and CHARACTER typed values
/// be extracted and passed as separate values.
llvm::SmallVector<mlir::Value, 4>
splitArguments(mlir::OpBuilder &builder, mlir::Location loc, mlir::Value arg) {
  mlir::Value zero;
  mlir::Value one;
  mlir::Type argTy = arg.getType();
  mlir::MLIRContext *context = argTy.getContext();
  const bool isComplex = argTy.isa<fir::CplxType>();
  const bool isCharLit = isCharacterLiteral(argTy);
  const bool isBoxChar = argTy.isa<fir::BoxCharType>();

  if (isComplex || isCharLit || isBoxChar) {
    // Only create these constants when needed and not every time
    zero = builder.create<mlir::ConstantOp>(loc, builder.getI64IntegerAttr(0));
    one = builder.create<mlir::ConstantOp>(loc, builder.getI64IntegerAttr(1));
  }
  if (isComplex) {
    auto eleTy =
        fir::RealType::get(context, argTy.cast<fir::CplxType>().getFKind());
    mlir::Value realPart =
        builder.create<fir::ExtractValueOp>(loc, eleTy, arg, zero);
    mlir::Value imaginaryPart =
        builder.create<fir::ExtractValueOp>(loc, eleTy, arg, one);
    return {realPart, imaginaryPart};
  }
  if (isBoxChar) {
    mlir::Type ptrTy =
        fir::ReferenceType::get(mlir::IntegerType::get(8, context));
    mlir::Type sizeTy = mlir::IntegerType::get(64, context);
    mlir::Value pointerPart =
        builder.create<fir::ExtractValueOp>(loc, ptrTy, arg, zero);
    mlir::Value sizePart =
        builder.create<fir::ExtractValueOp>(loc, sizeTy, arg, one);
    return {pointerPart, sizePart};
  }
  if (isCharLit) {
    mlir::Value variable = builder.create<fir::AllocaOp>(loc, argTy);
    builder.create<fir::StoreOp>(loc, arg, variable);
    mlir::Value sizePart = builder.create<mlir::ConstantOp>(
        loc, builder.getI64IntegerAttr(getLength(argTy)));
    return {variable, sizePart};
  }
  return {arg};
}

/// Generate a call to an Output I/O function.
/// The specific call is determined dynamically by the argument type.
void genOutputRuntimeFunc(mlir::OpBuilder &builder, mlir::Location loc,
                          mlir::Type argType, mlir::Value cookie,
                          llvm::SmallVector<mlir::Value, 4> &operands) {
  int i = 1;
  llvm::SmallVector<mlir::Value, 4> actuals{cookie};
  auto outputFunc = getOutputRuntimeFunc(builder, argType);

  for (auto &op : operands)
    actuals.emplace_back(builder.create<fir::ConvertOp>(
        loc, outputFunc.getType().getInput(i++), op));
  builder.create<mlir::CallOp>(loc, outputFunc, actuals);
}

/// Lower print statement assuming a dummy runtime interface for now.
void lowerPrintStatement(AbstractConverter &converter, mlir::Location loc,
                         mlir::ValueRange args,
                         const Fortran::parser::Format &format) {
  mlir::FuncOp beginFunc;
  mlir::OpBuilder &builder = converter.getOpBuilder();
  auto formatItems = lowerFormat(converter, format);
  if (formatItems.has_value()) {
    // has a format
    TODO();
  } else {
    beginFunc = getIORuntimeFunc<mkIOKey(BeginExternalListOutput)>(builder);
  }
  mlir::FunctionType beginFuncTy = beginFunc.getType();

  // Initiate io
  mlir::Value defaultUnit = builder.create<mlir::ConstantOp>(
      loc, builder.getIntegerAttr(beginFuncTy.getInput(0), 1));
  mlir::Value null =
      builder.create<mlir::ConstantOp>(loc, builder.getI64IntegerAttr(0));
  mlir::Value srcFileName =
      builder.create<fir::ConvertOp>(loc, beginFuncTy.getInput(1), null);
  mlir::Value lineNo = builder.create<mlir::ConstantOp>(
      loc, builder.getIntegerAttr(beginFuncTy.getInput(2), 0));
  llvm::SmallVector<mlir::Value, 3> beginArgs{defaultUnit, srcFileName, lineNo};
  mlir::Value cookie =
      builder.create<mlir::CallOp>(loc, beginFunc, beginArgs).getResult(0);

  // Call data transfer runtime function
  for (mlir::Value arg : args) {
    auto operands = splitArguments(builder, loc, arg);
    genOutputRuntimeFunc(builder, loc, arg.getType(), cookie, operands);
  }
  genEndIO(builder, loc, cookie);
}

llvm::SmallVector<mlir::Value, 4> lowerBeginArgsPositionOrFlush(
    AbstractConverter &converter, mlir::Location loc,
    const std::list<Fortran::parser::PositionOrFlushSpec> &specs) {
  llvm::SmallVector<mlir::Value, 4> args;
  // 1. find the unit number expression and append it
  for (auto &sp : specs)
    if (auto *un = std::get_if<Fortran::parser::FileUnitNumber>(&sp.u)) {
      auto *expr = Fortran::semantics::GetExpr(un->v);
      args.push_back(converter.genExprValue(expr, loc));
      break;
    }
  // 2 & 3. add the filename and line as extracted from `loc`
  // FIXME
  return args;
}

/// 3 of the 4 cases in a position (or flush) spec concern the error handling of
/// the statement. We handle those 3 cases here.
mlir::Value lowerErrorHandlingPositionOrFlush(
    AbstractConverter &converter, mlir::Value endRes,
    const std::list<Fortran::parser::PositionOrFlushSpec> &specs) {
  mlir::Value result;
  auto builder = converter.getOpBuilder();
  auto loc = converter.getCurrentLocation();
  for (auto &sp : specs) {
    std::visit(
        Fortran::common::visitors{
            [](const Fortran::parser::FileUnitNumber &) {
              // this is passed to the BeginFoo function
              // do nothing here
            },
            [&](const Fortran::parser::MsgVariable &var) {
              // call GetIoMsg, passing it the address of `var` and a length (in
              // bytes, or?). Effectively, we have to decompose a boxchar here.
              // TODO: this has to be a CHARACTER type, no?
              mlir::Value varAddr =
                  converter.genExprAddr(Fortran::semantics::GetExpr(var), loc);
              mlir::FuncOp getIoMsg =
                  getIORuntimeFunc<mkIOKey(GetIoMsg)>(builder);
              llvm::SmallVector<mlir::Value, 1> ioMsgArgs{
                  builder.create<fir::ConvertOp>(
                      loc, getModel<char *>()(builder.getContext()), varAddr)
                  /*, FIXME add length here */};
              builder.create<mlir::CallOp>(loc, getIoMsg, ioMsgArgs);
            },
            [&](const Fortran::parser::StatVariable &var) {
              /* store `endRes` to the variable `var` */
              mlir::Value varAddr =
                  converter.genExprAddr(Fortran::semantics::GetExpr(var), loc);
              builder.create<fir::StoreOp>(loc, endRes, varAddr);
            },
            [&](const Fortran::parser::ErrLabel &label) {
              /* pass the `endRes` value for `fir.switch` op */
              result = endRes;
            },
        },
        sp.u);
  }
  return result;
}

/// Generate IO calls for any of the "position or flush" like IO statements.
/// This is templatized with a statement type `S` and a key `K` for genericity.
template <typename K, typename S>
mlir::Value genPosOrFlushLikeStmt(AbstractConverter &converter, const S &stmt) {
  auto builder = converter.getOpBuilder();
  auto loc = converter.getCurrentLocation();
  auto beginFunc = getIORuntimeFunc<K>(builder);
  auto args = lowerBeginArgsPositionOrFlush(converter, loc, stmt.v);
  auto call = builder.create<mlir::CallOp>(loc, beginFunc, args);
  // FIXME: add call to EnableHandlers as apropos
  auto cookie = call.getResult(0);
  auto endVal = genEndIO(builder, converter.getCurrentLocation(), cookie);
  return lowerErrorHandlingPositionOrFlush(converter, endVal, stmt.v);
}
} // namespace

mlir::Value genBackspaceStatement(AbstractConverter &converter,
                                  const Fortran::parser::BackspaceStmt &stmt) {
  return genPosOrFlushLikeStmt<mkIOKey(BeginBackspace)>(converter, stmt);
}

mlir::Value genEndfileStatement(AbstractConverter &converter,
                                const Fortran::parser::EndfileStmt &stmt) {
  return genPosOrFlushLikeStmt<mkIOKey(BeginEndfile)>(converter, stmt);
}

mlir::Value genFlushStatement(AbstractConverter &converter,
                              const Fortran::parser::FlushStmt &stmt) {
  return genPosOrFlushLikeStmt<mkIOKey(BeginFlush)>(converter, stmt);
}

mlir::Value genRewindStatement(AbstractConverter &converter,
                               const Fortran::parser::RewindStmt &stmt) {
  return genPosOrFlushLikeStmt<mkIOKey(BeginRewind)>(converter, stmt);
}

mlir::Value genOpenStatement(AbstractConverter &converter,
                             const Fortran::parser::OpenStmt &) {
  auto builder = converter.getOpBuilder();
  mlir::FuncOp beginFunc;
  // if (...
  beginFunc = getIORuntimeFunc<mkIOKey(BeginOpenUnit)>(builder);
  // else
  beginFunc = getIORuntimeFunc<mkIOKey(BeginOpenNewUnit)>(builder);
  TODO();
  return {};
}

mlir::Value genCloseStatement(AbstractConverter &converter,
                              const Fortran::parser::CloseStmt &) {
  auto builder = converter.getOpBuilder();
  mlir::FuncOp beginFunc = getIORuntimeFunc<mkIOKey(BeginClose)>(builder);
  (void)beginFunc;
  TODO();
  return {};
}

void genPrintStatement(AbstractConverter &converter,
                       const Fortran::parser::PrintStmt &stmt) {
  llvm::SmallVector<mlir::Value, 4> args;
  for (auto &item : std::get<std::list<Fortran::parser::OutputItem>>(stmt.t)) {
    if (auto *pe{std::get_if<Fortran::parser::Expr>(&item.u)}) {
      auto loc = converter.genLocation(pe->source);
      args.push_back(
          converter.genExprValue(Fortran::semantics::GetExpr(*pe), loc));
    } else {
      TODO(); // TODO implied do
    }
  }
  lowerPrintStatement(converter, converter.getCurrentLocation(), args,
                      std::get<Fortran::parser::Format>(stmt.t));
}

mlir::Value genReadStatement(AbstractConverter &converter,
                             const Fortran::parser::ReadStmt &) {
  auto builder = converter.getOpBuilder();
  mlir::FuncOp beginFunc;
  // if (...
  beginFunc = getIORuntimeFunc<mkIOKey(BeginExternalListInput)>(builder);
  // else if (...
  TODO();
  return {};
}

mlir::Value genWriteStatement(AbstractConverter &converter,
                              const Fortran::parser::WriteStmt &) {
  auto builder = converter.getOpBuilder();
  mlir::FuncOp beginFunc;
  // if (...
  beginFunc = getIORuntimeFunc<mkIOKey(BeginExternalListOutput)>(builder);
  // else if (...
  TODO();
  return {};
}

mlir::Value genInquireStatement(AbstractConverter &converter,
                                const Fortran::parser::InquireStmt &) {
  auto builder = converter.getOpBuilder();
  mlir::FuncOp beginFunc;
  // if (...
  beginFunc = getIORuntimeFunc<mkIOKey(BeginInquireUnit)>(builder);
  // else if (...
  beginFunc = getIORuntimeFunc<mkIOKey(BeginInquireFile)>(builder);
  // else
  beginFunc = getIORuntimeFunc<mkIOKey(BeginInquireIoLength)>(builder);
  TODO();
  return {};
}

} // namespace Fortran::lower