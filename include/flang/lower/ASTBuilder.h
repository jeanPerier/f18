//===-- include/flang/lower/ASTBuilder.h ------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef FORTRAN_LOWER_AST_BUILDER_H_
#define FORTRAN_LOWER_AST_BUILDER_H_

#include "flang/parser/parse-tree.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>

/// Build a light-weight AST to help with lowering to FIR.  The AST will
/// capture pointers back into the parse tree, so the parse tree data structure
/// may <em>not</em> be changed between the construction of the AST and all of
/// its uses.
///
/// The AST captures a structured view of the program.  The program is a list of
/// units.  Function like units will contain lists of evaluations.  Evaluations
/// are either statements or constructs, where a construct contains a list of
/// evaluations.  The resulting AST structure can then be used to create FIR.

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

/// Flags to describe the impact of parse-trees nodes on the program
/// control flow. These annotations to parse-tree nodes are later used to
/// build the control flow graph when lowering to FIR.
enum class CFGAnnotation {
  None,            // Node does not impact control flow.
  Goto,            // Node acts like a goto on the control flow.
  CondGoto,        // Node acts like a conditional goto on the control flow.
  IndGoto,         // Node acts like an indirect goto on the control flow.
  IoSwitch,        // Node is an IO statement with ERR, END, or EOR specifier.
  Switch,          // Node acts like a switch on the control flow.
  Iterative,       // Node creates iterations in the control flow.
  FirStructuredOp, // Node is a structured loop.
  Return,          // Node triggers a return from the current procedure.
  Terminate        // Node terminates the program.
};

/// Compiler-generated jump
///
/// This is used to convert implicit control-flow edges to explicit form in the
/// decorated AST
struct CGJump {
  CGJump(Evaluation &to) : target{to} {}
  Evaluation &target;
};

/// is `A` a construct (or directive)?
template <typename A>
constexpr static bool isConstruct() {
  return std::is_same_v<A, parser::AssociateConstruct> ||
         std::is_same_v<A, parser::BlockConstruct> ||
         std::is_same_v<A, parser::CaseConstruct> ||
         std::is_same_v<A, parser::ChangeTeamConstruct> ||
         std::is_same_v<A, parser::CriticalConstruct> ||
         std::is_same_v<A, parser::DoConstruct> ||
         std::is_same_v<A, parser::IfConstruct> ||
         std::is_same_v<A, parser::SelectRankConstruct> ||
         std::is_same_v<A, parser::SelectTypeConstruct> ||
         std::is_same_v<A, parser::WhereConstruct> ||
         std::is_same_v<A, parser::ForallConstruct> ||
         std::is_same_v<A, parser::CompilerDirective> ||
         std::is_same_v<A, parser::OpenMPConstruct> ||
         std::is_same_v<A, parser::OmpEndLoopDirective>;
}

/// Function-like units can contains lists of evaluations.  These can be
/// (simple) statements or constructs, where a construct contains its own
/// evaluations.
struct Evaluation {
  using EvalVariant = std::variant<
      // action statements
      const parser::AllocateStmt *, const parser::AssignmentStmt *,
      const parser::BackspaceStmt *, const parser::CallStmt *,
      const parser::CloseStmt *, const parser::ContinueStmt *,
      const parser::CycleStmt *, const parser::DeallocateStmt *,
      const parser::EndfileStmt *, const parser::EventPostStmt *,
      const parser::EventWaitStmt *, const parser::ExitStmt *,
      const parser::FailImageStmt *, const parser::FlushStmt *,
      const parser::FormTeamStmt *, const parser::GotoStmt *,
      const parser::IfStmt *, const parser::InquireStmt *,
      const parser::LockStmt *, const parser::NullifyStmt *,
      const parser::OpenStmt *, const parser::PointerAssignmentStmt *,
      const parser::PrintStmt *, const parser::ReadStmt *,
      const parser::ReturnStmt *, const parser::RewindStmt *,
      const parser::StopStmt *, const parser::SyncAllStmt *,
      const parser::SyncImagesStmt *, const parser::SyncMemoryStmt *,
      const parser::SyncTeamStmt *, const parser::UnlockStmt *,
      const parser::WaitStmt *, const parser::WhereStmt *,
      const parser::WriteStmt *, const parser::ComputedGotoStmt *,
      const parser::ForallStmt *, const parser::ArithmeticIfStmt *,
      const parser::AssignStmt *, const parser::AssignedGotoStmt *,
      const parser::PauseStmt *,
      // compiler generated ops
      CGJump,
      // other statements
      const parser::FormatStmt *, const parser::EntryStmt *,
      const parser::DataStmt *, const parser::NamelistStmt *,
      // constructs
      const parser::AssociateConstruct *, const parser::BlockConstruct *,
      const parser::CaseConstruct *, const parser::ChangeTeamConstruct *,
      const parser::CriticalConstruct *, const parser::DoConstruct *,
      const parser::IfConstruct *, const parser::SelectRankConstruct *,
      const parser::SelectTypeConstruct *, const parser::WhereConstruct *,
      const parser::ForallConstruct *, const parser::CompilerDirective *,
      const parser::OpenMPConstruct *, const parser::OmpEndLoopDirective *,
      // construct statements
      const parser::AssociateStmt *, const parser::EndAssociateStmt *,
      const parser::BlockStmt *, const parser::EndBlockStmt *,
      const parser::SelectCaseStmt *, const parser::CaseStmt *,
      const parser::EndSelectStmt *, const parser::ChangeTeamStmt *,
      const parser::EndChangeTeamStmt *, const parser::CriticalStmt *,
      const parser::EndCriticalStmt *, const parser::NonLabelDoStmt *,
      const parser::EndDoStmt *, const parser::IfThenStmt *,
      const parser::ElseIfStmt *, const parser::ElseStmt *,
      const parser::EndIfStmt *, const parser::SelectRankStmt *,
      const parser::SelectRankCaseStmt *, const parser::SelectTypeStmt *,
      const parser::TypeGuardStmt *, const parser::WhereConstructStmt *,
      const parser::MaskedElsewhereStmt *, const parser::ElsewhereStmt *,
      const parser::EndWhereStmt *, const parser::ForallConstructStmt *,
      const parser::EndForallStmt *>;

  Evaluation() = delete;
  Evaluation(const Evaluation &) = delete;
  Evaluation(Evaluation &&) = default;

  /// General ctor
  template <typename A>
  Evaluation(const A &a, const ParentType &p, const parser::CharBlock &pos,
             const std::optional<parser::Label> &lab)
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
    return std::is_same_v<A, parser::FormatStmt> ||
           std::is_same_v<A, parser::EntryStmt> ||
           std::is_same_v<A, parser::DataStmt> ||
           std::is_same_v<A, parser::NamelistStmt>;
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
    return isStmt() ? nullptr : subs.get();
  }

  /// Set that the construct `cstr` (if not a nullptr) has branches.
  static void setBranches(Evaluation *cstr) {
    if (cstr)
      cstr->setBranches();
  }

  EvalVariant u;
  ParentType parent;
  parser::CharBlock pos;
  std::optional<parser::Label> lab;
  std::unique_ptr<EvaluationCollection> subs; // construct sub-statements
  CFGAnnotation cfg{CFGAnnotation::None};
  bool isTarget{false};         // this evaluation is a control target
  bool containsBranches{false}; // construct contains branches
};

/// A program is a list of program units.
/// These units can be function like, module like, or block data
struct ProgramUnit {
  template <typename A>
  ProgramUnit(A *ptr, const ParentType &parent) : p{ptr}, parent{parent} {}
  ProgramUnit(ProgramUnit &&) = default;
  ProgramUnit(const ProgramUnit &) = delete;

  std::variant<const parser::MainProgram *, const parser::FunctionSubprogram *,
               const parser::SubroutineSubprogram *, const parser::Module *,
               const parser::Submodule *,
               const parser::SeparateModuleSubprogram *,
               const parser::BlockData *>
      p;
  ParentType parent;
};

/// Function-like units have similar structure. They all can contain executable
/// statements as well as other function-like units (internal procedures and
/// function statements).
struct FunctionLikeUnit : public ProgramUnit {
  // wrapper statements for function-like syntactic structures
  using FunctionStatement =
      std::variant<const parser::Statement<parser::ProgramStmt> *,
                   const parser::Statement<parser::EndProgramStmt> *,
                   const parser::Statement<parser::FunctionStmt> *,
                   const parser::Statement<parser::EndFunctionStmt> *,
                   const parser::Statement<parser::SubroutineStmt> *,
                   const parser::Statement<parser::EndSubroutineStmt> *,
                   const parser::Statement<parser::MpSubprogramStmt> *,
                   const parser::Statement<parser::EndMpSubprogramStmt> *>;

  FunctionLikeUnit(const parser::MainProgram &f, const ParentType &parent);
  FunctionLikeUnit(const parser::FunctionSubprogram &f,
                   const ParentType &parent);
  FunctionLikeUnit(const parser::SubroutineSubprogram &f,
                   const ParentType &parent);
  FunctionLikeUnit(const parser::SeparateModuleSubprogram &f,
                   const ParentType &parent);
  FunctionLikeUnit(FunctionLikeUnit &&) = default;
  FunctionLikeUnit(const FunctionLikeUnit &) = delete;

  bool isMainProgram() {
    return std::holds_alternative<
        const parser::Statement<parser::EndProgramStmt> *>(funStmts.back());
  }
  const parser::FunctionStmt *getFunction() {
    return getA<parser::FunctionStmt>();
  }
  const parser::SubroutineStmt *getSubroutine() {
    return getA<parser::SubroutineStmt>();
  }
  const parser::MpSubprogramStmt *getMPSubp() {
    return getA<parser::MpSubprogramStmt>();
  }

  std::list<FunctionStatement> funStmts; // begin/end pair
  EvaluationCollection evals;            // statements
  std::list<FunctionLikeUnit> funcs;     // internal procedures

private:
  template <typename A>
  const A *getA() {
    if (auto p = std::get_if<const parser::Statement<A> *>(&funStmts.front()))
      return &(*p)->statement;
    return nullptr;
  }
};

/// Module-like units have similar structure. They all can contain a list of
/// function-like units.
struct ModuleLikeUnit : public ProgramUnit {
  // wrapper statements for module-like syntactic structures
  using ModuleStatement =
      std::variant<const parser::Statement<parser::ModuleStmt> *,
                   const parser::Statement<parser::EndModuleStmt> *,
                   const parser::Statement<parser::SubmoduleStmt> *,
                   const parser::Statement<parser::EndSubmoduleStmt> *>;

  ModuleLikeUnit(const parser::Module &m, const ParentType &parent);
  ModuleLikeUnit(const parser::Submodule &m, const ParentType &parent);
  ~ModuleLikeUnit() = default;
  ModuleLikeUnit(ModuleLikeUnit &&) = default;
  ModuleLikeUnit(const ModuleLikeUnit &) = delete;

  std::list<ModuleStatement> modStmts;
  std::list<FunctionLikeUnit> funcs;
};

struct BlockDataUnit : public ProgramUnit {
  BlockDataUnit(const parser::BlockData &bd, const ParentType &parent);
  BlockDataUnit(BlockDataUnit &&) = default;
  BlockDataUnit(const BlockDataUnit &) = delete;
};

/// A Program is the top-level AST
struct Program {
  using Units = std::variant<FunctionLikeUnit, ModuleLikeUnit, BlockDataUnit>;

  Program() = default;
  Program(Program &&) = default;
  Program(const Program &) = delete;

  std::list<Units> &getUnits() { return units; }

private:
  std::list<Units> units;
};

} // namespace AST

/// Create an AST from the parse tree
std::unique_ptr<AST::Program> createAST(const parser::Program &root);

/// Decorate the AST with control flow annotations
///
/// The AST must be decorated with control-flow annotations to prepare it for
/// use in generating a CFG-like structure.
void annotateControl(AST::Program &ast);

void dumpAST(llvm::raw_ostream &o, AST::Program &ast);

} // namespace Fortran::lower

#endif // FORTRAN_LOWER_AST_BUILDER_H_
