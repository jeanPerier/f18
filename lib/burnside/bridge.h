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

#ifndef FORTRAN_BURNSIDE_BRIDGE_H_
#define FORTRAN_BURNSIDE_BRIDGE_H_

#include "mlir/IR/MLIRContext.h"
#include "mlir/IR/Module.h"
#include <memory>

/// Implement the burnside bridge from Fortran to
/// [MLIR](https://github.com/tensorflow/mlir).
///
/// [Coding style](https://llvm.org/docs/CodingStandards.html)

namespace Fortran::common {
class IntrinsicTypeDefaultKinds;
}

namespace Fortran::parser {
class CookedSource;
struct Program;
}

namespace llvm {
class Module;
class SourceMgr;
}

namespace fir {
struct NameMangler;
}

namespace Fortran::burnside {

class BurnsideBridge {
public:
  static BurnsideBridge create(
      const common::IntrinsicTypeDefaultKinds &defaultKinds,
      const parser::CookedSource *cooked) {
    return BurnsideBridge{defaultKinds, cooked};
  }

  mlir::MLIRContext &getMLIRContext() { return *context.get(); }
  mlir::ModuleOp &getModule() { return *module.get(); }

  void parseSourceFile(llvm::SourceMgr &);

  common::IntrinsicTypeDefaultKinds const &getDefaultKinds() {
    return defaultKinds;
  }

  bool validModule() { return getModule(); }

  const parser::CookedSource *getCookedSource() const { return cooked; }

private:
  explicit BurnsideBridge(const common::IntrinsicTypeDefaultKinds &defaultKinds,
      const parser::CookedSource *cooked);
  BurnsideBridge() = delete;
  BurnsideBridge(const BurnsideBridge &) = delete;

  const common::IntrinsicTypeDefaultKinds &defaultKinds;
  const parser::CookedSource *cooked;
  std::unique_ptr<mlir::MLIRContext> context;
  std::unique_ptr<mlir::ModuleOp> module;
};

/// Cross the bridge from the Fortran parse-tree, etc. to FIR+OpenMP+MLIR
void crossBurnsideBridge(BurnsideBridge &bridge, const parser::Program &program,
    fir::NameMangler &mangler);

/// Bridge from MLIR to LLVM-IR
std::unique_ptr<llvm::Module> LLVMBridge(mlir::ModuleOp &module);

/// Get an instance of the Burnside bridge
BurnsideBridge getBurnsideBridge(
    const common::IntrinsicTypeDefaultKinds &defaultKinds,
    const parser::CookedSource *cooked = nullptr);

}  // Fortran::burnside

#endif  // FORTRAN_BURNSIDE_BRIDGE_H_
