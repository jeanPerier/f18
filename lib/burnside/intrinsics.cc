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

#include "intrinsics.h"
#include "builder.h"
#include "fir/FIROps.h"

/// [Coding style](https://llvm.org/docs/CodingStandards.html)

namespace Fortran::burnside {

// Define a simple static runtime description that will be transformed into
// RuntimeFunction when building the IntrinsicLibrary.
namespace runtime {
enum class Type { f32, f64 };
struct RuntimeStaticDescription {
  const char *name;
  const char *symbol;
  Type resultType;
  std::vector<Type> argumentTypes; // TODO make this a constexpr friendly table
};

// TODO
// This table should be generated in a clever ways and probably shared with
// lib/evaluate intrinsic folding.
static const RuntimeStaticDescription llvm[] = {
    {"abs", "llvm.fabs.f32", Type::f32, {Type::f32}},
    {"abs", "llvm.fabs.f64", Type::f64, {Type::f64}},
    {"acos", "acosf", Type::f32, {Type::f32}},
//    {"acos", "acos", Type::f64, {Type::f64}},
    {"atan", "atan2f", Type::f32, {Type::f32, Type::f32}},
    {"atan", "atan2", Type::f64, {Type::f64, Type::f64}},
    {"sqrt", "llvm.sqrt.f32", Type::f32, {Type::f32}},
    {"sqrt", "llvm.sqrt.f64", Type::f64, {Type::f64}},
    {"cos", "llvm.cos.f32", Type::f32, {Type::f32}},
    {"cos", "llvm.cos.f64", Type::f64, {Type::f64}},
    {"sin", "llvm.sin.f32", Type::f32, {Type::f32}},
    {"sin", "llvm.sin.f64", Type::f64, {Type::f64}},
};



// Conversion between types of the static representation and MLIR types.
mlir::Type toMLIRType(Type t, mlir::MLIRContext &context) {
  switch (t) {
  case Type::f32: return mlir::FloatType::getF32(&context);
  case Type::f64: return mlir::FloatType::getF64(&context);
  }
}
mlir::FunctionType toMLIRFunctionType(
    const RuntimeStaticDescription &func, mlir::MLIRContext &context) {
  std::vector<mlir::Type> argMLIRTypes;
  for (runtime::Type t : func.argumentTypes) {
    argMLIRTypes.push_back(toMLIRType(t, context));
  }
  return mlir::FunctionType::get(
      argMLIRTypes, toMLIRType(func.resultType, context), &context);
}
}  // runtime

namespace test {
// TODO wrap codegenerators as class members to simplify arg passing
using CodeGenerator = mlir::Value*(*)(mlir::OpBuilder&,const IntrinsicLibrary&, mlir::Type, llvm::ArrayRef<mlir::Value*>, llvm::StringRef);
struct IntrinsicHanlder {
  const char *name;
  CodeGenerator opGenerator{nullptr};
};

mlir::FunctionType getFunctionType(mlir::Type resultType, llvm::ArrayRef<mlir::Value*> arguments, mlir::MLIRContext* context) {
    llvm::SmallVector<mlir::Type, 2> argumentTypes;
    for (const auto& arg : arguments) {
      assert(arg != nullptr); // TODO think about optionals
      argumentTypes.push_back(arg->getType());
    }
    return mlir::FunctionType::get(argumentTypes, resultType, context);
}

// TODO find nicer type to string infra
llvm::StringRef typeToString(mlir::Type type) {
  if (type.isF16()) {
    return "f16";
  } else if (type.isF32()) {
    return "f32";
  } else if (type.isF64()){
    return "f64";
  } else {
    return "unkown";
  }
}

mlir::Value* OutlineInWrapper(mlir::OpBuilder& builder, const IntrinsicLibrary& library, mlir::Type resultType, llvm::ArrayRef<mlir::Value*> arguments, llvm::StringRef name, CodeGenerator codeGen) {
    mlir::ModuleOp module{getModule(&builder)};
    mlir::MLIRContext* context{module.getContext()};
    auto loc{mlir::UnknownLoc::get(context)};
    // Only if injecting code in a wrapper function
    auto funcType{getFunctionType(resultType, arguments, context)};
    std::string wrapperName{"fir."+name.str()+"."+typeToString(resultType).str()};
    mlir::FuncOp function{getNamedFunction(module, wrapperName)};
    if (!function) {
      // First time this wrapper is needed, build it.
      function = createFunction(module, wrapperName, funcType);
      function.setAttr("fir.intrinsic", builder.getUnitAttr());
      function.addEntryBlock();
      auto localBuilder{std::make_unique<mlir::OpBuilder>(function)};
      localBuilder->setInsertionPointToStart(&function.front());
      llvm::SmallVector<mlir::Value*, 2> localArguments;
      for (mlir::BlockArgument* bArg: function.front().getArguments()) {
        localArguments.push_back(bArg);
      }
      auto result{codeGen(*localBuilder, library, resultType, localArguments, name)};
      localBuilder->create<mlir::ReturnOp>(loc, result);
    } else {
      // Wrapper was already built, ensure it has the sought type
      assert(function.getType() == funcType);
    }
    auto call{builder.create<mlir::CallOp>(loc, function, arguments)};
    return call.getResult(0);
}

mlir::Value* LowerToRuntime(mlir::OpBuilder& builder, const IntrinsicLibrary& library, mlir::Type resultType, llvm::ArrayRef<mlir::Value*> arguments, llvm::StringRef name) {
    // TODO optional are not handled
    mlir::ModuleOp module{getModule(&builder)};
    mlir::MLIRContext* context{module.getContext()};
    auto loc{mlir::UnknownLoc::get(context)};

    // Look up runtime
    mlir::FunctionType soughtFuncType{getFunctionType(resultType, arguments, context)};
    if (auto funcOp{library.getFunction(name, soughtFuncType, builder)}) {
      mlir::FunctionType actualFuncType{funcOp->getType()};
      if (actualFuncType.getNumResults() != soughtFuncType.getNumResults() ||
          actualFuncType.getNumInputs() != soughtFuncType.getNumInputs() ||
          actualFuncType.getNumInputs() != arguments.size() ||
          actualFuncType.getNumResults() != 1) {
        assert(false); //TODO return optional
      }
      llvm::SmallVector<mlir::Value*, 2> convertedArguments;
      int i{0};
      for (mlir::Value* arg : arguments) {
        mlir::Type actualType{actualFuncType.getInput(i)};
        if (soughtFuncType.getInput(i) != actualType) {
          auto castedArg{builder.create<fir::ConvertOp>(loc, actualType, arg)};
          convertedArguments.push_back(castedArg.getResult());
        } else {
          convertedArguments.push_back(arg);
        }
      }
      auto call{builder.create<mlir::CallOp>(loc, *funcOp, convertedArguments)};
      mlir::Type soughtType{soughtFuncType.getResult(0)};
      mlir::Value* res{call.getResult(0)};
      if (actualFuncType.getResult(0) != soughtType) {
        auto castedRes{builder.create<fir::ConvertOp>(loc, soughtType, res)};
        return castedRes.getResult();
      } else {
        return res;
      }
    } else {
      // could not find runtime function
      assert(false); // TODO: better error handling.
    }
}

mlir::Value* LowerToRuntimeInWrapper(mlir::OpBuilder& builder, const IntrinsicLibrary& library, mlir::Type resultType, llvm::ArrayRef<mlir::Value*> arguments, llvm::StringRef name) {
  return OutlineInWrapper(builder, library, resultType, arguments, name, &LowerToRuntime);
}

static IntrinsicHanlder handlers[] {
  {"abs", LowerToRuntimeInWrapper},
  {"acos", LowerToRuntime},
  {"atan", LowerToRuntimeInWrapper},
  {"sqrt", LowerToRuntimeInWrapper},
  {"cos", LowerToRuntimeInWrapper},
  {"sin", LowerToRuntimeInWrapper},
};

} // test TODO wrap in a class or something


mlir::Value* IntrinsicLibrary::LowerIntrinsic(llvm::StringRef name, mlir::Type resultType, llvm::ArrayRef<mlir::Value*> args, mlir::OpBuilder &builder) const {
  for (const test::IntrinsicHanlder& handler : test::handlers) {
    if (name == handler.name) {
      assert(handler.opGenerator != nullptr);
      return handler.opGenerator(builder, *(this), resultType, args, name);
    }
  }
  return nullptr;
}

mlir::FuncOp getFuncOp(mlir::OpBuilder& builder, const IntrinsicLibrary::RuntimeFunction& runtime) {
  auto module{getModule(&builder)};
  mlir::FuncOp function{getNamedFunction(module, runtime.symbol)};
  if (!function) {
    function = createFunction(module, runtime.symbol, runtime.type);
    function.setAttr("fir.runtime", builder.getUnitAttr());
  }
  return function;
}

std::optional<mlir::FuncOp> IntrinsicLibrary::getFunction(
    llvm::StringRef name, mlir::FunctionType funcType, mlir::OpBuilder &builder) const {
  auto range{lib.equal_range(name)};
  const RuntimeFunction* bestNearMatch{nullptr};
  for (auto iter{range.first}; iter != range.second; ++iter ) {
    const RuntimeFunction& impl{iter->second};
    if (funcType == impl.type) {
      return getFuncOp(builder, impl); // exact match
    } else {
      if (!bestNearMatch && impl.type.getNumResults() == funcType.getNumResults()
          && impl.type.getNumInputs() == funcType.getNumInputs()) {
        bestNearMatch = &impl;
      } else {
        // TODO the best near match should not be the first hit.
        // It should apply rules:
        //  -> Non narrowing argument conversion are better
        //  -> The "nearest" conversions are better
      }
    }
  }
  if (bestNearMatch != nullptr) {
    return getFuncOp(builder, *bestNearMatch);
  } else {
    return std::nullopt;
  }
}

// So far ignore the version an only load the dummy llvm lib.
IntrinsicLibrary IntrinsicLibrary::create(
    IntrinsicLibrary::Version, mlir::MLIRContext &context) {
  Map map;
  for (const auto &func : runtime::llvm) {
    IntrinsicLibrary::Key key{func.name};
    RuntimeFunction impl{func.symbol, runtime::toMLIRFunctionType(func, context)};
    map.insert({key, impl});
  }
  return IntrinsicLibrary{std::move(map)};
}


////////////////////// Refactored

class MathRuntimeLibrary {
public:
  using Key = llvm::StringRef;
  struct Hash { // Need custom hash for this kind of key
    size_t operator()(const Key &k) const { return llvm::hash_value(k); }
  };
  struct RuntimeFunction {
    RuntimeFunction(llvm::StringRef n, mlir::FunctionType t)
      : symbol{n}, type{t} {};
    llvm::StringRef symbol;
    mlir::FunctionType type;
  };
  using Map = std::unordered_multimap<Key, RuntimeFunction, Hash>;

  MathRuntimeLibrary(IntrinsicLibraryNew::Version, mlir::MLIRContext&);
  /// Probe the intrinsic library for a certain intrinsic and get/build the
  /// related mlir::FuncOp if a runtime description is found.
  /// Also add an unit attribute "fir.intrinsic" to the function so that later
  /// it is possible to quickly know what function are intrinsics vs users.
  //  TODO put builder first in signature
  std::optional<mlir::FuncOp> getFunction(
      llvm::StringRef, mlir::FunctionType, mlir::OpBuilder &) const;

  /// Create the runtime description for the targeted library version.

private:
  mlir::FuncOp getFuncOp(mlir::OpBuilder& builder, const RuntimeFunction& runtime) const;
  Map library;
};

class IntrinsicGeneratorContext {
public:
  using Generator = mlir::Value* (IntrinsicGeneratorContext::*)();
  mlir::Value* defaultGenerator(){return outlineInWrapper(&I::mapToRuntime); }
  template<Generator g> mlir::Value* outline() { return outlineInWrapper(g); }
  mlir::Value* mapToRuntime() {return nullptr;};
  
private:
  struct IntrinsicHanlder {
    const char *name;
    Generator generator{&I::defaultGenerator};
  };
  using I = IntrinsicGeneratorContext;
  // Table that drives the fir generation depending on the intrinsic.
  // The default is to attempt to replace by a runtime call with
  // one to one mapping with Fortran arguments. Type cast are
  // inserted if no runtime function with the sought type is available.
  static constexpr IntrinsicHanlder handlers[] {
    {"abs", &I::defaultGenerator},
    {"acos", &I::defaultGenerator},
    {"atan", &I::defaultGenerator},
    {"sqrt", &I::defaultGenerator},
    {"cos", &I::defaultGenerator},
    {"sin", &I::defaultGenerator},
  };
  // helpers
  std::string getWrapperName();
  mlir::Value* outlineInWrapper(Generator);

  // Info required by all the Generators
  mlir::Location loc;
  mlir::OpBuilder& builder;
  llvm::StringRef name;
  mlir::Type resultType;
  llvm::ArrayRef<mlir::Value*> arguments;
  mlir::FunctionType funcType;
  MathRuntimeLibrary& runtime;
};

class IntrinsicLibraryNew::Implementation {
public:
  Implementation(Version v, mlir::MLIRContext& c): runtime{v, c} {}
  inline mlir::Value* genval(mlir::Location loc, mlir::OpBuilder &builder, llvm::StringRef name, mlir::Type resultType, llvm::ArrayRef<mlir::Value*> args);
private:
  MathRuntimeLibrary runtime;
};

// IntrinsicLibraryNew implementation

IntrinsicLibraryNew IntrinsicLibraryNew::create(IntrinsicLibraryNew::Version v, mlir::MLIRContext& context) {
  IntrinsicLibraryNew lib{};
  lib.impl = new Implementation(v, context);
  return lib;
}

IntrinsicLibraryNew::~IntrinsicLibraryNew() { delete impl; }

mlir::Value* IntrinsicLibraryNew::genval(mlir::Location loc, mlir::OpBuilder &builder, llvm::StringRef name, mlir::Type resultType, llvm::ArrayRef<mlir::Value*> args) {
  assert(impl);
  return impl->genval(loc, builder, name, resultType, args);
}

// Implementation implementation
//
mlir::Value* IntrinsicLibraryNew::Implementation::genval(mlir::Location loc, mlir::OpBuilder &builder, llvm::StringRef name, mlir::Type resultType, llvm::ArrayRef<mlir::Value*> args) {
  // TODO
  return nullptr;
}
  
// MathRuntimeLibrary implementation

MathRuntimeLibrary::MathRuntimeLibrary(IntrinsicLibraryNew::Version, mlir::MLIRContext& context) {
  for (const auto &func : runtime::llvm) {
    Key key{func.name};
    RuntimeFunction impl{func.symbol, runtime::toMLIRFunctionType(func, context)};
    library.insert({key, impl});
  }
}

mlir::FuncOp MathRuntimeLibrary::getFuncOp(mlir::OpBuilder& builder, const RuntimeFunction& runtime) const {
  auto module{getModule(&builder)};
  mlir::FuncOp function{getNamedFunction(module, runtime.symbol)};
  if (!function) {
    function = createFunction(module, runtime.symbol, runtime.type);
    function.setAttr("fir.runtime", builder.getUnitAttr());
  }
  return function;
}

std::optional<mlir::FuncOp> MathRuntimeLibrary::getFunction(
    llvm::StringRef name, mlir::FunctionType funcType, mlir::OpBuilder &builder) const {
  auto range{library.equal_range(name)};
  const RuntimeFunction* bestNearMatch{nullptr};
  for (auto iter{range.first}; iter != range.second; ++iter ) {
    const RuntimeFunction& impl{iter->second};
    if (funcType == impl.type) {
      return getFuncOp(builder, impl); // exact match
    } else {
      if (!bestNearMatch && impl.type.getNumResults() == funcType.getNumResults()
          && impl.type.getNumInputs() == funcType.getNumInputs()) {
        bestNearMatch = &impl;
      } else {
        // TODO the best near match should not be the first hit.
        // It should apply rules:
        //  -> Non narrowing argument conversion are better
        //  -> The "nearest" conversions are better
      }
    }
  }
  if (bestNearMatch != nullptr) {
    return getFuncOp(builder, *bestNearMatch);
  } else {
    return std::nullopt;
  }
}


//
//
//
//
//

}
