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

#ifndef FORTRAN_BURNSIDE_CANONICALIZE_H_
#define FORTRAN_BURNSIDE_CANONICALIZE_H_

/// [Coding style](https://llvm.org/docs/CodingStandards.html)

namespace mlir {
class Location;
class OpBuilder;
class Type;
class Value;
}

namespace fir {
class AllocaExpr;
}

namespace Fortran {
namespace evaluate {
template<typename> class Expr;
struct SomeType;
}  // evaluate
namespace semantics {
class Symbol;
}

namespace burnside {

class SymMap;

mlir::Value *createSomeExpression(mlir::Location loc, mlir::OpBuilder &builder,
    const Fortran::evaluate::Expr<Fortran::evaluate::SomeType> &expr,
    SymMap &symMap);
mlir::Value *createSomeAddress(mlir::Location loc, mlir::OpBuilder &builder,
    const Fortran::evaluate::Expr<Fortran::evaluate::SomeType> &expr,
    SymMap &symMap);

mlir::Value *createTemporary(mlir::Location loc, mlir::OpBuilder &builder,
    SymMap &symMap, mlir::Type type, const semantics::Symbol *symbol);

}  // burnside
}  // Fortran

#endif  // FORTRAN_BURNSIDE_CANONICALIZE_H_
