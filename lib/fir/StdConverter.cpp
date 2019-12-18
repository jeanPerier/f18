// Copyright (c) 2019, NVIDIA CORPORATION.  All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include "fir/Transforms/StdConverter.h"
#include "fir/Attribute.h"
#include "fir/FIRDialect.h"
#include "fir/FIROpsSupport.h"
#include "fir/FIRType.h"
#include "mlir/Conversion/AffineToStandard/AffineToStandard.h"
#include "mlir/Dialect/StandardOps/Ops.h"
#include "mlir/IR/StandardTypes.h"
#include "mlir/Pass/Pass.h"
#include "mlir/Transforms/DialectConversion.h"
#include "llvm/ADT/ArrayRef.h"

// This module performs the conversion of FIR operations to MLIR standard and/or
// LLVM-IR dialects.

namespace L = llvm;
namespace M = mlir;

using namespace fir;

static L::cl::opt<bool>
    ClDisableFirToStd("disable-fir2std",
                      L::cl::desc("disable FIR to standard pass"),
                      L::cl::init(false), L::cl::Hidden);

namespace {

using SmallVecResult = L::SmallVector<M::Value *, 4>;
using OperandTy = L::ArrayRef<M::Value *>;
using AttributeTy = L::ArrayRef<M::NamedAttribute>;

/// FIR to standard type converter
/// This converts a subset of FIR types to standard types
class FIRToStdTypeConverter : public M::TypeConverter {
public:
  using TypeConverter::TypeConverter;

  // convert a front-end kind value to either a std dialect type
  // FIXME: use KindMapping
  static M::Type kindToRealType(M::MLIRContext *ctx, KindTy kind) {
    switch (kind) {
    case 2:
      return M::FloatType::getF16(ctx);
    case 3:
      return M::FloatType::getBF16(ctx);
    case 4:
      return M::FloatType::getF32(ctx);
    case 8:
      return M::FloatType::getF64(ctx);
    }
    return fir::RealType::get(ctx, kind);
  }

  /// Convert FIR types to MLIR standard dialect types
  M::Type convertType(M::Type t) override {
    if (auto cplx = t.dyn_cast<CplxType>())
      return M::ComplexType::get(
          kindToRealType(cplx.getContext(), cplx.getFKind()));
    if (auto integer = t.dyn_cast<IntType>())
      return M::IntegerType::get(integer.getFKind() * 8, integer.getContext());
    if (auto real = t.dyn_cast<RealType>())
      return kindToRealType(real.getContext(), real.getFKind());
    return t;
  }
};

/// FIR conversion pattern template
template <typename FromOp>
class FIROpConversion : public M::ConversionPattern {
public:
  explicit FIROpConversion(M::MLIRContext *ctx, FIRToStdTypeConverter &lowering)
      : ConversionPattern(FromOp::getOperationName(), 1, ctx),
        lowering(lowering) {}

protected:
  M::Type convertType(M::Type ty) const { return lowering.convertType(ty); }

  FIRToStdTypeConverter &lowering;
};

/// SelectTypeOp converted to an if-then-else chain
///
/// This lowers the test conditions to calls into the runtime
struct SelectTypeOpConversion : public FIROpConversion<SelectTypeOp> {
  using FIROpConversion::FIROpConversion;

  M::PatternMatchResult
  matchAndRewrite(M::Operation *op, OperandTy operands,
                  L::ArrayRef<M::Block *> destinations,
                  L::ArrayRef<OperandTy> destOperands,
                  M::ConversionPatternRewriter &rewriter) const override {
    auto selecttype = M::cast<SelectTypeOp>(op);
    auto conds = selecttype.getNumConditions();
    auto attrName = SelectTypeOp::AttrName;
    auto caseAttr = selecttype.getAttrOfType<M::ArrayAttr>(attrName);
    auto cases = caseAttr.getValue();
    // Selector must be of type !fir.box<T>
    auto &selector = operands[0];
    auto loc = selecttype.getLoc();
    auto mod = op->getParentOfType<M::ModuleOp>();
    for (unsigned t = 0; t != conds; ++t) {
      auto &attr = cases[t];
      if (auto a = attr.dyn_cast_or_null<fir::ExactTypeAttr>()) {
        genTypeLadderStep(loc, true, selector, a.getType(), destinations[t],
                          destOperands[t], mod, rewriter);
        continue;
      }
      if (auto a = attr.dyn_cast_or_null<fir::SubclassAttr>()) {
        genTypeLadderStep(loc, false, selector, a.getType(), destinations[t],
                          destOperands[t], mod, rewriter);
        continue;
      }
      assert(attr.dyn_cast_or_null<M::UnitAttr>());
      assert((t + 1 == conds) && "unit must be last");
      rewriter.replaceOpWithNewOp<M::BranchOp>(selecttype, destinations[t],
                                               M::ValueRange{destOperands[t]});
    }
    return matchSuccess();
  }

  static void lookupFunction(L::StringRef name, M::FunctionType type,
                             M::ModuleOp module,
                             M::ConversionPatternRewriter &rewriter) {
    fir::createFuncOp(rewriter.getUnknownLoc(), module, name, type);
  }

  static void genTypeLadderStep(M::Location loc, bool exactTest,
                                M::Value *selector, M::Type ty, M::Block *dest,
                                OperandTy destOps, M::ModuleOp module,
                                M::ConversionPatternRewriter &rewriter) {
    M::Type tydesc = fir::TypeDescType::get(ty);
    M::Value *t = rewriter.create<GenTypeDescOp>(loc, M::TypeAttr::get(tydesc));
    std::vector<M::Value *> actuals = {selector, t};
    auto fty = rewriter.getI1Type();
    std::vector<M::Type> argTy = {fir::BoxType::get(rewriter.getNoneType()),
                                  tydesc};
    L::StringRef funName =
        exactTest ? "FIXME_exact_type_match" : "FIXME_isa_type_test";
    lookupFunction(funName, rewriter.getFunctionType(argTy, fty), module,
                   rewriter);
    // FIXME: need to call actual runtime routines for (1) testing if the
    // runtime type of the selector is an exact match to a derived type or (2)
    // testing if the runtime type of the selector is a derived type or one of
    // that derived type's subtypes.
    auto cmp = rewriter.create<M::CallOp>(
        loc, fty, rewriter.getSymbolRefAttr(funName), actuals);
    auto *thisBlock = rewriter.getInsertionBlock();
    auto *newBlock = rewriter.createBlock(dest);
    rewriter.setInsertionPointToEnd(thisBlock);
    rewriter.create<M::CondBranchOp>(loc, cmp.getResult(0), dest, destOps,
                                     newBlock, OperandTy{});
    rewriter.setInsertionPointToEnd(newBlock);
  }
};

/// Convert affine dialect, fir.select_type to standard dialect
class FIRToStdLoweringPass : public M::FunctionPass<FIRToStdLoweringPass> {
public:
  void runOnFunction() override {
    if (ClDisableFirToStd)
      return;

    auto *context{&getContext()};
    FIRToStdTypeConverter typeConverter;
    M::OwningRewritePatternList patterns;
    patterns.insert<SelectTypeOpConversion>(context, typeConverter);
    M::populateAffineToStdConversionPatterns(patterns, context);
    M::populateFuncOpTypeConversionPattern(patterns, context, typeConverter);
    M::ConversionTarget target{*context};
    target.addLegalDialect<M::StandardOpsDialect, fir::FIROpsDialect>();
    target.addDynamicallyLegalOp<M::FuncOp>([&](M::FuncOp op) {
      return typeConverter.isSignatureLegal(op.getType());
    });
    target.addIllegalOp<SelectTypeOp>();
    if (M::failed(M::applyPartialConversion(
            getModule(), target, std::move(patterns), &typeConverter))) {
      M::emitError(M::UnknownLoc::get(context),
                   "error in converting to standard dialect\n");
      signalPassFailure();
    }
  }

  M::ModuleOp getModule() {
    return getFunction().getParentOfType<M::ModuleOp>();
  }
};

} // namespace

std::unique_ptr<M::Pass> fir::createFIRToStdPass() {
  return std::make_unique<FIRToStdLoweringPass>();
}
