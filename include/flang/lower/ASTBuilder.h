//===-- include/flang/lower/AstBuilder.h ------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef FORTRAN_LOWER_AST_BUILDER_H_
#define FORTRAN_LOWER_AST_BUILDER_H_

#include "flang/parser/parse-tree.h"
#include "flang/semantics/scope.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>

namespace Fortran::lower {
namespace AST {

struct Evaluation;
struct Program;
struct ModuleLikeUnit;
struct FunctionLikeUnit;

// TODO: A collection of Evaluations can obviously be any of the container
// types; leaving this as a std::list _for now_ because we reserve the right to
// insert AST nodes in any order in O(1) time.
using EvaluationCollection = std::list<Evaluation>;

using ParentType =
    std::variant<Program *, ModuleLikeUnit *, FunctionLikeUnit *, Evaluation *>;

enum class CFGAnnotation {
  None,
  Goto,
  CondGoto,
  IndGoto,
  IoSwitch,
  Switch,
  Iterative,
  FirStructuredOp,
  Return,
  Terminate
};

/// Compiler-generated jump
///
/// This is used to convert implicit control-flow edges to explicit form in the
/// decorated AST
struct CGJump {
  CGJump(Evaluation *to) : target{to} {}
  Evaluation *target{nullptr};
};

/// is `A` a construct (or directive)?
template <typename A>
constexpr static bool isConstruct() {
  return std::is_same_v<A, Fortran::parser::AssociateConstruct> ||
         std::is_same_v<A, Fortran::parser::BlockConstruct> ||
         std::is_same_v<A, Fortran::parser::CaseConstruct> ||
         std::is_same_v<A, Fortran::parser::ChangeTeamConstruct> ||
         std::is_same_v<A, Fortran::parser::CriticalConstruct> ||
         std::is_same_v<A, Fortran::parser::DoConstruct> ||
         std::is_same_v<A, Fortran::parser::IfConstruct> ||
         std::is_same_v<A, Fortran::parser::SelectRankConstruct> ||
         std::is_same_v<A, Fortran::parser::SelectTypeConstruct> ||
         std::is_same_v<A, Fortran::parser::WhereConstruct> ||
         std::is_same_v<A, Fortran::parser::ForallConstruct> ||
         std::is_same_v<A, Fortran::parser::CompilerDirective> ||
         std::is_same_v<A, Fortran::parser::OpenMPConstruct> ||
         std::is_same_v<A, Fortran::parser::OmpEndLoopDirective>;
}

/// Function-like units can contains lists of evaluations.  These can be
/// (simple) statements or constructs, where a construct contains its own
/// evaluations.
struct Evaluation {
  using EvalVariant = std::variant<
      // action statements
      const Fortran::parser::AllocateStmt *,
      const Fortran::parser::AssignmentStmt *,
      const Fortran::parser::BackspaceStmt *, const Fortran::parser::CallStmt *,
      const Fortran::parser::CloseStmt *, const Fortran::parser::ContinueStmt *,
      const Fortran::parser::CycleStmt *,
      const Fortran::parser::DeallocateStmt *,
      const Fortran::parser::EndfileStmt *,
      const Fortran::parser::EventPostStmt *,
      const Fortran::parser::EventWaitStmt *, const Fortran::parser::ExitStmt *,
      const Fortran::parser::FailImageStmt *,
      const Fortran::parser::FlushStmt *, const Fortran::parser::FormTeamStmt *,
      const Fortran::parser::GotoStmt *, const Fortran::parser::IfStmt *,
      const Fortran::parser::InquireStmt *, const Fortran::parser::LockStmt *,
      const Fortran::parser::NullifyStmt *, const Fortran::parser::OpenStmt *,
      const Fortran::parser::PointerAssignmentStmt *,
      const Fortran::parser::PrintStmt *, const Fortran::parser::ReadStmt *,
      const Fortran::parser::ReturnStmt *, const Fortran::parser::RewindStmt *,
      const Fortran::parser::StopStmt *, const Fortran::parser::SyncAllStmt *,
      const Fortran::parser::SyncImagesStmt *,
      const Fortran::parser::SyncMemoryStmt *,
      const Fortran::parser::SyncTeamStmt *,
      const Fortran::parser::UnlockStmt *, const Fortran::parser::WaitStmt *,
      const Fortran::parser::WhereStmt *, const Fortran::parser::WriteStmt *,
      const Fortran::parser::ComputedGotoStmt *,
      const Fortran::parser::ForallStmt *,
      const Fortran::parser::ArithmeticIfStmt *,
      const Fortran::parser::AssignStmt *,
      const Fortran::parser::AssignedGotoStmt *,
      const Fortran::parser::PauseStmt *,
      // compiler generated ops
      CGJump,
      // other statements
      const Fortran::parser::FormatStmt *, const Fortran::parser::EntryStmt *,
      const Fortran::parser::DataStmt *, const Fortran::parser::NamelistStmt *,
      // constructs
      const Fortran::parser::AssociateConstruct *,
      const Fortran::parser::BlockConstruct *,
      const Fortran::parser::CaseConstruct *,
      const Fortran::parser::ChangeTeamConstruct *,
      const Fortran::parser::CriticalConstruct *,
      const Fortran::parser::DoConstruct *,
      const Fortran::parser::IfConstruct *,
      const Fortran::parser::SelectRankConstruct *,
      const Fortran::parser::SelectTypeConstruct *,
      const Fortran::parser::WhereConstruct *,
      const Fortran::parser::ForallConstruct *,
      const Fortran::parser::CompilerDirective *,
      const Fortran::parser::OpenMPConstruct *,
      const Fortran::parser::OmpEndLoopDirective *,
      // construct statements
      const Fortran::parser::AssociateStmt *,
      const Fortran::parser::EndAssociateStmt *,
      const Fortran::parser::BlockStmt *, const Fortran::parser::EndBlockStmt *,
      const Fortran::parser::SelectCaseStmt *,
      const Fortran::parser::CaseStmt *, const Fortran::parser::EndSelectStmt *,
      const Fortran::parser::ChangeTeamStmt *,
      const Fortran::parser::EndChangeTeamStmt *,
      const Fortran::parser::CriticalStmt *,
      const Fortran::parser::EndCriticalStmt *,
      const Fortran::parser::NonLabelDoStmt *,
      const Fortran::parser::EndDoStmt *, const Fortran::parser::IfThenStmt *,
      const Fortran::parser::ElseIfStmt *, const Fortran::parser::ElseStmt *,
      const Fortran::parser::EndIfStmt *,
      const Fortran::parser::SelectRankStmt *,
      const Fortran::parser::SelectRankCaseStmt *,
      const Fortran::parser::SelectTypeStmt *,
      const Fortran::parser::TypeGuardStmt *,
      const Fortran::parser::WhereConstructStmt *,
      const Fortran::parser::MaskedElsewhereStmt *,
      const Fortran::parser::ElsewhereStmt *,
      const Fortran::parser::EndWhereStmt *,
      const Fortran::parser::ForallConstructStmt *,
      const Fortran::parser::EndForallStmt *>;

  Evaluation() = delete;
  Evaluation(const Evaluation &) = default;

  /// General ctor
  template <typename A>
  Evaluation(const A &a, const ParentType &p,
             const Fortran::parser::CharBlock &pos,
             const std::optional<Fortran::parser::Label> &lab)
      : u{&a}, parent{p}, pos{pos}, lab{lab} {}

  /// Compiler-generated jump
  Evaluation(const CGJump &jump, const ParentType &p)
      : u{jump}, parent{p}, cfg{CFGAnnotation::Goto} {}

  /// Construct ctor
  template <typename A>
  Evaluation(const A &a, const ParentType &parent) : u{&a}, parent{parent} {
    static_assert(AST::isConstruct<A>(), "must be a construct");
  }

  /// is `A` an action statement ?
  template <typename A>
  constexpr static bool isActionStmt(const A &a) {
    return !AST::isConstruct<A>() && !isOther(a);
  }

  /// is `A` a compiler-generated evaluation?
  template <typename A>
  constexpr static bool isGenerated(const A &) {
    return std::is_same_v<A, CGJump>;
  }

  /// is `A` not an executable statement?
  template <typename A>
  constexpr static bool isOther(const A &) {
    return std::is_same_v<A, Fortran::parser::FormatStmt> ||
           std::is_same_v<A, Fortran::parser::EntryStmt> ||
           std::is_same_v<A, Fortran::parser::DataStmt> ||
           std::is_same_v<A, Fortran::parser::NamelistStmt>;
  }

  constexpr bool isActionOrGenerated() const {
    return std::visit(common::visitors{
                          [](auto *p) { return isActionStmt(*p); },
                          [](auto &r) { return isGenerated(r); },
                      },
                      u);
  }

  constexpr bool isStmt() const {
    return std::visit(
        common::visitors{
            [](auto *p) { return isActionStmt(*p) || isOther(*p); },
            [](auto &r) { return isGenerated(r); },
        },
        u);
  }
  constexpr bool isConstruct() const { return !isStmt(); }

  /// Set the type of originating control flow type for this evaluation.
  void setCFG(CFGAnnotation a, Evaluation *cstr) {
    cfg = a;
    setBranches(cstr);
  }

  /// Is this evaluation a control-flow origin? (The AST must be annotated)
  bool isControlOrigin() const { return cfg != CFGAnnotation::None; }

  /// Is this evaluation a control-flow target? (The AST must be annotated)
  bool isControlTarget() const { return isTarget; }

  /// Set the containsBranches flag iff this evaluation (a construct) contains
  /// control flow
  void setBranches() { containsBranches = true; }

  constexpr EvaluationCollection *getConstructEvals() {
    return isStmt() ? nullptr : subs;
  }

  /// Set that the construct `cstr` (if not a nullptr) has branches.
  static void setBranches(Evaluation *cstr) {
    if (cstr)
      cstr->setBranches();
  }

  EvalVariant u;
  ParentType parent;
  Fortran::parser::CharBlock pos;
  std::optional<Fortran::parser::Label> lab;
  EvaluationCollection *subs; // construct sub-statements
  CFGAnnotation cfg{CFGAnnotation::None};
  bool isTarget{false};         // this evaluation is a control target
  bool containsBranches{false}; // construct contains branches
};

/// A program is a list of program units.
/// These units can be function like, module like, or block data
struct ProgramUnit {
  template <typename A>
  ProgramUnit(A *ptr, const ParentType &parent) : p{ptr}, parent{parent} {}

  std::variant<const Fortran::parser::MainProgram *,
               const Fortran::parser::FunctionSubprogram *,
               const Fortran::parser::SubroutineSubprogram *,
               const Fortran::parser::Module *,
               const Fortran::parser::Submodule *,
               const Fortran::parser::SeparateModuleSubprogram *,
               const Fortran::parser::BlockData *>
      p;
  ParentType parent;
};

/// Function-like units have similar structure. They all can contain executable
/// statements.
struct FunctionLikeUnit : public ProgramUnit {
  // wrapper statements for function-like syntactic structures
  using FunctionStatement = std::variant<
      const Fortran::parser::Statement<Fortran::parser::ProgramStmt> *,
      const Fortran::parser::Statement<Fortran::parser::EndProgramStmt> *,
      const Fortran::parser::Statement<Fortran::parser::FunctionStmt> *,
      const Fortran::parser::Statement<Fortran::parser::EndFunctionStmt> *,
      const Fortran::parser::Statement<Fortran::parser::SubroutineStmt> *,
      const Fortran::parser::Statement<Fortran::parser::EndSubroutineStmt> *,
      const Fortran::parser::Statement<Fortran::parser::MpSubprogramStmt> *,
      const Fortran::parser::Statement<Fortran::parser::EndMpSubprogramStmt> *>;

  FunctionLikeUnit(const Fortran::parser::MainProgram &f,
                   const ParentType &parent);
  FunctionLikeUnit(const Fortran::parser::FunctionSubprogram &f,
                   const ParentType &parent);
  FunctionLikeUnit(const Fortran::parser::SubroutineSubprogram &f,
                   const ParentType &parent);
  FunctionLikeUnit(const Fortran::parser::SeparateModuleSubprogram &f,
                   const ParentType &parent);

  bool isMainProgram() {
    return std::holds_alternative<
        const Fortran::parser::Statement<Fortran::parser::EndProgramStmt> *>(
        funStmts.back());
  }
  const Fortran::parser::FunctionStmt *getFunction() {
    return getA<Fortran::parser::FunctionStmt>();
  }
  const Fortran::parser::SubroutineStmt *getSubroutine() {
    return getA<Fortran::parser::SubroutineStmt>();
  }
  const Fortran::parser::MpSubprogramStmt *getMPSubp() {
    return getA<Fortran::parser::MpSubprogramStmt>();
  }

  const semantics::Scope *scope{nullptr}; // scope from front-end
  std::list<FunctionStatement> funStmts;  // begin/end pair
  EvaluationCollection evals;             // statements
  std::list<FunctionLikeUnit> funcs;      // internal procedures

private:
  template <typename A>
  const A *getA() {
    if (auto p = std::get_if<const Fortran::parser::Statement<A> *>(
            &funStmts.front()))
      return &(*p)->statement;
    return nullptr;
  }
};

/// Module-like units have similar structure. They all can contain a list of
/// function-like units.
struct ModuleLikeUnit : public ProgramUnit {
  // wrapper statements for module-like syntactic structures
  using ModuleStatement = std::variant<
      const Fortran::parser::Statement<Fortran::parser::ModuleStmt> *,
      const Fortran::parser::Statement<Fortran::parser::EndModuleStmt> *,
      const Fortran::parser::Statement<Fortran::parser::SubmoduleStmt> *,
      const Fortran::parser::Statement<Fortran::parser::EndSubmoduleStmt> *>;

  ModuleLikeUnit(const Fortran::parser::Module &m, const ParentType &parent);
  ModuleLikeUnit(const Fortran::parser::Submodule &m, const ParentType &parent);
  ~ModuleLikeUnit() = default;

  const semantics::Scope *scope{nullptr};
  std::list<ModuleStatement> modStmts;
  std::list<FunctionLikeUnit> funcs;
};

struct BlockDataUnit : public ProgramUnit {
  BlockDataUnit(const Fortran::parser::BlockData &bd, const ParentType &parent);
};

/// A Program is the top-level AST
struct Program {
  using Units = std::variant<FunctionLikeUnit, ModuleLikeUnit, BlockDataUnit>;

  std::list<Units> &getUnits() { return units; }

private:
  std::list<Units> units;
};

} // namespace AST

/// Create an AST from the parse tree
std::unique_ptr<AST::Program> createAST(const Fortran::parser::Program &root);

/// Decorate the AST with control flow annotations
///
/// The AST must be decorated with control-flow annotations to prepare it for
/// use in generating a CFG-like structure.
void annotateControl(AST::Program &ast);

void dumpAST(llvm::raw_ostream &o, AST::Program &ast);

} // namespace Fortran::lower

#endif // FORTRAN_LOWER_AST_BUILDER_H_
