//===-- lib/lower/AstBuilder.cc -------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "flang/lower/ASTBuilder.h"
#include "flang/parser/dump-parse-tree.h"
#include "flang/parser/parse-tree-visitor.h"
#include <algorithm>
#include <cassert>
#include <utility>

/// Build a light-weight AST to help with lowering to FIR.  The AST will
/// capture pointers back into the parse tree, so the parse tree data structure
/// may <em>not</em> be changed between the construction of the AST and all of
/// its uses.
///
/// The AST captures a structured view of the program.  The program is a list of
/// units.  Function like units will contain lists of evaluations.  Evaluations
/// are either statements or constructs, where a construct contains a list of
/// evaluations.  The resulting AST structure can then be used to create FIR.

namespace {

/// The instantiation of a parse tree visitor (Pre and Post) is extremely
/// expensive in terms of compile and link time, so one goal here is to limit
/// the bridge to one such instantiation.
class ASTBuilder {
public:
  ASTBuilder() {
    pgm = new Fortran::lower::AST::Program;
    parents.push_back(pgm);
  }

  /// Get the result
  Fortran::lower::AST::Program *result() { return pgm; }

  template <typename A>
  constexpr bool Pre(const A &) {
    return true;
  }
  template <typename A>
  constexpr void Post(const A &) {}

  // Module like

  bool Pre(const Fortran::parser::Module &node) { return enterModule(node); }
  bool Pre(const Fortran::parser::Submodule &node) { return enterModule(node); }

  void Post(const Fortran::parser::Module &) { exitModule(); }
  void Post(const Fortran::parser::Submodule &) { exitModule(); }

  // Function like

  bool Pre(const Fortran::parser::MainProgram &node) { return enterFunc(node); }
  bool Pre(const Fortran::parser::FunctionSubprogram &node) {
    return enterFunc(node);
  }
  bool Pre(const Fortran::parser::SubroutineSubprogram &node) {
    return enterFunc(node);
  }
  bool Pre(const Fortran::parser::SeparateModuleSubprogram &node) {
    return enterFunc(node);
  }

  void Post(const Fortran::parser::MainProgram &) { exitFunc(); }
  void Post(const Fortran::parser::FunctionSubprogram &) { exitFunc(); }
  void Post(const Fortran::parser::SubroutineSubprogram &) { exitFunc(); }
  void Post(const Fortran::parser::SeparateModuleSubprogram &) { exitFunc(); }

  // Block data

  void Post(const Fortran::parser::BlockData &node) {
    Fortran::lower::AST::BlockDataUnit unit{node, parents.back()};
    addUnit(unit);
  }

  //
  // Action statements
  //

  void Post(const Fortran::parser::Statement<Fortran::parser::ActionStmt> &s) {
    addEval(makeEvalAction(s));
  }
  void
  Post(const Fortran::parser::UnlabeledStatement<Fortran::parser::ActionStmt>
           &s) {
    addEval(makeEvalAction(s));
  }

  //
  // Non-executable statements
  //

  void
  Post(const Fortran::parser::Statement<
       Fortran::common::Indirection<Fortran::parser::FormatStmt>> &statement) {
    addEval(makeEvalIndirect(statement));
  }
  void
  Post(const Fortran::parser::Statement<
       Fortran::common::Indirection<Fortran::parser::EntryStmt>> &statement) {
    addEval(makeEvalIndirect(statement));
  }
  void
  Post(const Fortran::parser::Statement<
       Fortran::common::Indirection<Fortran::parser::DataStmt>> &statement) {
    addEval(makeEvalIndirect(statement));
  }
  void Post(const Fortran::parser::Statement<Fortran::common::Indirection<
                Fortran::parser::NamelistStmt>> &statement) {
    addEval(makeEvalIndirect(statement));
  }

  //
  // Construct statements
  //

  void Post(const Fortran::parser::Statement<Fortran::parser::AssociateStmt>
                &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const Fortran::parser::Statement<Fortran::parser::EndAssociateStmt>
                &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(
      const Fortran::parser::Statement<Fortran::parser::BlockStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const Fortran::parser::Statement<Fortran::parser::EndBlockStmt>
                &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const Fortran::parser::Statement<Fortran::parser::SelectCaseStmt>
                &statement) {
    addEval(makeEvalDirect(statement));
  }
  void
  Post(const Fortran::parser::Statement<Fortran::parser::CaseStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const Fortran::parser::Statement<Fortran::parser::EndSelectStmt>
                &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const Fortran::parser::Statement<Fortran::parser::ChangeTeamStmt>
                &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const Fortran::parser::Statement<Fortran::parser::EndChangeTeamStmt>
                &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const Fortran::parser::Statement<Fortran::parser::CriticalStmt>
                &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const Fortran::parser::Statement<Fortran::parser::EndCriticalStmt>
                &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const Fortran::parser::Statement<Fortran::parser::NonLabelDoStmt>
                &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(
      const Fortran::parser::Statement<Fortran::parser::EndDoStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const Fortran::parser::Statement<Fortran::parser::IfThenStmt>
                &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const Fortran::parser::Statement<Fortran::parser::ElseIfStmt>
                &statement) {
    addEval(makeEvalDirect(statement));
  }
  void
  Post(const Fortran::parser::Statement<Fortran::parser::ElseStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(
      const Fortran::parser::Statement<Fortran::parser::EndIfStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const Fortran::parser::Statement<Fortran::parser::SelectRankStmt>
                &statement) {
    addEval(makeEvalDirect(statement));
  }
  void
  Post(const Fortran::parser::Statement<Fortran::parser::SelectRankCaseStmt>
           &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const Fortran::parser::Statement<Fortran::parser::SelectTypeStmt>
                &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const Fortran::parser::Statement<Fortran::parser::TypeGuardStmt>
                &statement) {
    addEval(makeEvalDirect(statement));
  }
  void
  Post(const Fortran::parser::Statement<Fortran::parser::WhereConstructStmt>
           &statement) {
    addEval(makeEvalDirect(statement));
  }
  void
  Post(const Fortran::parser::Statement<Fortran::parser::MaskedElsewhereStmt>
           &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const Fortran::parser::Statement<Fortran::parser::ElsewhereStmt>
                &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const Fortran::parser::Statement<Fortran::parser::EndWhereStmt>
                &statement) {
    addEval(makeEvalDirect(statement));
  }
  void
  Post(const Fortran::parser::Statement<Fortran::parser::ForallConstructStmt>
           &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const Fortran::parser::Statement<Fortran::parser::EndForallStmt>
                &statement) {
    addEval(makeEvalDirect(statement));
  }
  // Get rid of production wrapper
  void Post(const Fortran::parser::UnlabeledStatement<
            Fortran::parser::ForallAssignmentStmt> &statement) {
    addEval(std::visit(
        [&](const auto &x) {
          return Fortran::lower::AST::Evaluation{
              x, parents.back(), statement.source, {}};
        },
        statement.statement.u));
  }
  void
  Post(const Fortran::parser::Statement<Fortran::parser::ForallAssignmentStmt>
           &statement) {
    addEval(std::visit(
        [&](const auto &x) {
          return Fortran::lower::AST::Evaluation{
              x, parents.back(), statement.source, statement.label};
        },
        statement.statement.u));
  }

  //
  // Constructs (enter and exit)
  //

  bool Pre(const Fortran::parser::AssociateConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const Fortran::parser::BlockConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const Fortran::parser::CaseConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const Fortran::parser::ChangeTeamConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const Fortran::parser::CriticalConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const Fortran::parser::DoConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const Fortran::parser::IfConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const Fortran::parser::SelectRankConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const Fortran::parser::SelectTypeConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const Fortran::parser::WhereConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const Fortran::parser::ForallConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const Fortran::parser::CompilerDirective &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const Fortran::parser::OpenMPConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const Fortran::parser::OmpEndLoopDirective &construct) {
    return enterConstruct(construct);
  }

  void Post(const Fortran::parser::AssociateConstruct &) { exitConstruct(); }
  void Post(const Fortran::parser::BlockConstruct &) { exitConstruct(); }
  void Post(const Fortran::parser::CaseConstruct &) { exitConstruct(); }
  void Post(const Fortran::parser::ChangeTeamConstruct &) { exitConstruct(); }
  void Post(const Fortran::parser::CriticalConstruct &) { exitConstruct(); }
  void Post(const Fortran::parser::DoConstruct &) { exitConstruct(); }
  void Post(const Fortran::parser::IfConstruct &) { exitConstruct(); }
  void Post(const Fortran::parser::SelectRankConstruct &) { exitConstruct(); }
  void Post(const Fortran::parser::SelectTypeConstruct &) { exitConstruct(); }
  void Post(const Fortran::parser::WhereConstruct &) { exitConstruct(); }
  void Post(const Fortran::parser::ForallConstruct &) { exitConstruct(); }
  void Post(const Fortran::parser::CompilerDirective &) { exitConstruct(); }
  void Post(const Fortran::parser::OpenMPConstruct &) { exitConstruct(); }
  void Post(const Fortran::parser::OmpEndLoopDirective &) { exitConstruct(); }

private:
  // ActionStmt has a couple of non-conforming cases, which get handled
  // explicitly here.  The other cases use an Indirection, which we discard in
  // the AST.
  Fortran::lower::AST::Evaluation
  makeEvalAction(const Fortran::parser::Statement<Fortran::parser::ActionStmt>
                     &statement) {
    return std::visit(
        Fortran::common::visitors{
            [&](const Fortran::parser::ContinueStmt &x) {
              return Fortran::lower::AST::Evaluation{
                  x, parents.back(), statement.source, statement.label};
            },
            [&](const Fortran::parser::FailImageStmt &x) {
              return Fortran::lower::AST::Evaluation{
                  x, parents.back(), statement.source, statement.label};
            },
            [&](const auto &x) {
              return Fortran::lower::AST::Evaluation{
                  x.value(), parents.back(), statement.source, statement.label};
            },
        },
        statement.statement.u);
  }
  Fortran::lower::AST::Evaluation makeEvalAction(
      const Fortran::parser::UnlabeledStatement<Fortran::parser::ActionStmt>
          &statement) {
    return std::visit(
        Fortran::common::visitors{
            [&](const Fortran::parser::ContinueStmt &x) {
              return Fortran::lower::AST::Evaluation{
                  x, parents.back(), statement.source, {}};
            },
            [&](const Fortran::parser::FailImageStmt &x) {
              return Fortran::lower::AST::Evaluation{
                  x, parents.back(), statement.source, {}};
            },
            [&](const auto &x) {
              return Fortran::lower::AST::Evaluation{
                  x.value(), parents.back(), statement.source, {}};
            },
        },
        statement.statement.u);
  }

  template <typename A>
  Fortran::lower::AST::Evaluation makeEvalIndirect(
      const Fortran::parser::Statement<Fortran::common::Indirection<A>>
          &statement) {
    return Fortran::lower::AST::Evaluation{statement.statement.value(),
                                           parents.back(), statement.source,
                                           statement.label};
  }

  template <typename A>
  Fortran::lower::AST::Evaluation
  makeEvalDirect(const Fortran::parser::Statement<A> &statement) {
    return Fortran::lower::AST::Evaluation{statement.statement, parents.back(),
                                           statement.source, statement.label};
  }

  // When we enter a function-like structure, we want to build a new unit and
  // set the builder's cursors to point to it.
  template <typename A>
  bool enterFunc(const A &func) {
    auto &unit =
        addFunc(Fortran::lower::AST::FunctionLikeUnit{func, parents.back()});
    funclist = &unit.funcs;
    pushEval(&unit.evals);
    parents.emplace_back(&unit);
    return true;
  }

  void exitFunc() {
    popEval();
    funclist = nullptr;
    parents.pop_back();
  }

  // When we enter a construct structure, we want to build a new construct and
  // set the builder's evaluation cursor to point to it.
  template <typename A>
  bool enterConstruct(const A &construct) {
    auto &con =
        addEval(Fortran::lower::AST::Evaluation{construct, parents.back()});
    con.subs = new Fortran::lower::AST::EvaluationCollection;
    pushEval(con.subs);
    parents.emplace_back(&con);
    return true;
  }

  void exitConstruct() {
    popEval();
    parents.pop_back();
  }

  // When we enter a module structure, we want to build a new module and
  // set the builder's function cursor to point to it.
  template <typename A>
  bool enterModule(const A &func) {
    auto &unit =
        addUnit(Fortran::lower::AST::ModuleLikeUnit{func, parents.back()});
    funclist = &unit.funcs;
    parents.emplace_back(&unit);
    return true;
  }

  void exitModule() {
    funclist = nullptr;
    parents.pop_back();
  }

  template <typename A>
  A &addUnit(const A &unit) {
    pgm->getUnits().emplace_back(unit);
    return std::get<A>(pgm->getUnits().back());
  }

  template <typename A>
  A &addFunc(const A &func) {
    if (funclist) {
      funclist->emplace_back(func);
      return funclist->back();
    }
    return addUnit(func);
  }

  /// move the Evaluation to the end of the current list
  Fortran::lower::AST::Evaluation &
  addEval(Fortran::lower::AST::Evaluation &&eval) {
    assert(funclist && "not in a function");
    assert(evallist.size() > 0);
    evallist.back()->emplace_back(std::move(eval));
    return evallist.back()->back();
  }

  /// push a new list on the stack of Evaluation lists
  void pushEval(Fortran::lower::AST::EvaluationCollection *eval) {
    assert(funclist && "not in a function");
    assert(eval && eval->empty() && "evaluation list isn't correct");
    evallist.emplace_back(eval);
  }

  /// pop the current list and return to the last Evaluation list
  void popEval() {
    assert(funclist && "not in a function");
    evallist.pop_back();
  }

  Fortran::lower::AST::Program *pgm;
  std::list<Fortran::lower::AST::FunctionLikeUnit> *funclist{nullptr};
  std::vector<Fortran::lower::AST::EvaluationCollection *> evallist;
  std::vector<Fortran::lower::AST::ParentType> parents;
};

template <typename A>
constexpr bool hasErrLabel(const A &stmt) {
  auto isError{[](const auto &v) {
    return std::holds_alternative<Fortran::parser::ErrLabel>(v.u);
  }};
  if constexpr (std::is_same_v<A, Fortran::parser::ReadStmt> ||
                std::is_same_v<A, Fortran::parser::WriteStmt>) {
    return std::any_of(std::begin(stmt.controls), std::end(stmt.controls),
                       isError);
  }
  if constexpr (std::is_same_v<A, Fortran::parser::WaitStmt> ||
                std::is_same_v<A, Fortran::parser::OpenStmt> ||
                std::is_same_v<A, Fortran::parser::CloseStmt> ||
                std::is_same_v<A, Fortran::parser::BackspaceStmt> ||
                std::is_same_v<A, Fortran::parser::EndfileStmt> ||
                std::is_same_v<A, Fortran::parser::RewindStmt> ||
                std::is_same_v<A, Fortran::parser::FlushStmt>) {
    return std::any_of(std::begin(stmt.v), std::end(stmt.v), isError);
  }
  if constexpr (std::is_same_v<A, Fortran::parser::InquireStmt>) {
    const auto &specifiers{
        std::get<std::list<Fortran::parser::InquireSpec>>(stmt.u)};
    return std::any_of(std::begin(specifiers), std::end(specifiers), isError);
  }
  return false;
}

template <typename A>
constexpr bool hasEorLabel(const A &stmt) {
  if constexpr (std::is_same_v<A, Fortran::parser::ReadStmt> ||
                std::is_same_v<A, Fortran::parser::WriteStmt>) {
    for (const auto &control : stmt.controls) {
      if (std::holds_alternative<Fortran::parser::EorLabel>(control.u))
        return true;
    }
  }
  if constexpr (std::is_same_v<A, Fortran::parser::WaitStmt>) {
    for (const auto &waitSpec : stmt.v) {
      if (std::holds_alternative<Fortran::parser::EorLabel>(waitSpec.u))
        return true;
    }
  }
  return false;
}

template <typename A>
constexpr bool hasEndLabel(const A &stmt) {
  if constexpr (std::is_same_v<A, Fortran::parser::ReadStmt> ||
                std::is_same_v<A, Fortran::parser::WriteStmt>) {
    for (const auto &control : stmt.controls) {
      if (std::holds_alternative<Fortran::parser::EndLabel>(control.u))
        return true;
    }
  }
  if constexpr (std::is_same_v<A, Fortran::parser::WaitStmt>) {
    for (const auto &waitSpec : stmt.v) {
      if (std::holds_alternative<Fortran::parser::EndLabel>(waitSpec.u))
        return true;
    }
  }
  return false;
}

bool hasAltReturns(const Fortran::parser::CallStmt &callStmt) {
  const auto &args{
      std::get<std::list<Fortran::parser::ActualArgSpec>>(callStmt.v.t)};
  for (const auto &arg : args) {
    const auto &actual{std::get<Fortran::parser::ActualArg>(arg.t)};
    if (std::holds_alternative<Fortran::parser::AltReturnSpec>(actual.u))
      return true;
  }
  return false;
}

/// Determine if `callStmt` has alternate returns and if so set `e` to be the
/// origin of a switch-like control flow
void altRet(Fortran::lower::AST::Evaluation &evaluation,
            const Fortran::parser::CallStmt *callStmt,
            Fortran::lower::AST::Evaluation *cstr) {
  if (hasAltReturns(*callStmt))
    evaluation.setCFG(Fortran::lower::AST::CFGAnnotation::Switch, cstr);
}

template <typename A>
void ioLabel(Fortran::lower::AST::Evaluation &evaluation, const A *statement,
             Fortran::lower::AST::Evaluation *cstr) {
  if (hasErrLabel(*statement) || hasEorLabel(*statement) ||
      hasEndLabel(*statement))
    evaluation.setCFG(Fortran::lower::AST::CFGAnnotation::IoSwitch, cstr);
}

void annotateEvalListCFG(
    Fortran::lower::AST::EvaluationCollection &evaluationCollection,
    Fortran::lower::AST::Evaluation *cstr) {
  bool nextIsTarget = false;
  for (auto &eval : evaluationCollection) {
    eval.isTarget = nextIsTarget;
    nextIsTarget = false;
    if (eval.isConstruct()) {
      annotateEvalListCFG(*eval.getConstructEvals(), &eval);
      // assume that the entry and exit are both possible branch targets
      nextIsTarget = true;
    }
    if (eval.isActionOrGenerated() && eval.lab.has_value())
      eval.isTarget = true;
    std::visit(
        Fortran::common::visitors{
            [&](const Fortran::parser::BackspaceStmt *statement) {
              ioLabel(eval, statement, cstr);
            },
            [&](const Fortran::parser::CallStmt *statement) {
              altRet(eval, statement, cstr);
            },
            [&](const Fortran::parser::CloseStmt *statement) {
              ioLabel(eval, statement, cstr);
            },
            [&](const Fortran::parser::CycleStmt *) {
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::Goto, cstr);
            },
            [&](const Fortran::parser::EndfileStmt *statement) {
              ioLabel(eval, statement, cstr);
            },
            [&](const Fortran::parser::ExitStmt *) {
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::Goto, cstr);
            },
            [&](const Fortran::parser::FailImageStmt *) {
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::Terminate, cstr);
            },
            [&](const Fortran::parser::FlushStmt *statement) {
              ioLabel(eval, statement, cstr);
            },
            [&](const Fortran::parser::GotoStmt *) {
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::Goto, cstr);
            },
            [&](const Fortran::parser::IfStmt *) {
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::CondGoto, cstr);
            },
            [&](const Fortran::parser::InquireStmt *statement) {
              ioLabel(eval, statement, cstr);
            },
            [&](const Fortran::parser::OpenStmt *statement) {
              ioLabel(eval, statement, cstr);
            },
            [&](const Fortran::parser::ReadStmt *statement) {
              ioLabel(eval, statement, cstr);
            },
            [&](const Fortran::parser::ReturnStmt *) {
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::Return, cstr);
            },
            [&](const Fortran::parser::RewindStmt *statement) {
              ioLabel(eval, statement, cstr);
            },
            [&](const Fortran::parser::StopStmt *) {
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::Terminate, cstr);
            },
            [&](const Fortran::parser::WaitStmt *statement) {
              ioLabel(eval, statement, cstr);
            },
            [&](const Fortran::parser::WriteStmt *statement) {
              ioLabel(eval, statement, cstr);
            },
            [&](const Fortran::parser::ArithmeticIfStmt *) {
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::Switch, cstr);
            },
            [&](const Fortran::parser::AssignedGotoStmt *) {
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::IndGoto, cstr);
            },
            [&](const Fortran::parser::ComputedGotoStmt *) {
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::Switch, cstr);
            },
            [&](const Fortran::parser::WhereStmt *) {
              // fir.loop + fir.where around the next stmt
              eval.isTarget = true;
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::Iterative, cstr);
            },
            [&](const Fortran::parser::ForallStmt *) {
              // fir.loop around the next stmt
              eval.isTarget = true;
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::Iterative, cstr);
            },
            [&](Fortran::lower::AST::CGJump &) {
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::Goto, cstr);
            },
            [&](const Fortran::parser::EndAssociateStmt *) {
              eval.isTarget = true;
            },
            [&](const Fortran::parser::EndBlockStmt *) {
              eval.isTarget = true;
            },
            [&](const Fortran::parser::SelectCaseStmt *) {
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::Switch, cstr);
            },
            [&](const Fortran::parser::CaseStmt *) { eval.isTarget = true; },
            [&](const Fortran::parser::EndSelectStmt *) {
              eval.isTarget = true;
            },
            [&](const Fortran::parser::EndChangeTeamStmt *) {
              eval.isTarget = true;
            },
            [&](const Fortran::parser::EndCriticalStmt *) {
              eval.isTarget = true;
            },
            [&](const Fortran::parser::NonLabelDoStmt *) {
              eval.isTarget = true;
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::Iterative, cstr);
            },
            [&](const Fortran::parser::EndDoStmt *) {
              eval.isTarget = true;
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::Goto, cstr);
            },
            [&](const Fortran::parser::IfThenStmt *) {
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::CondGoto, cstr);
            },
            [&](const Fortran::parser::ElseIfStmt *) {
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::CondGoto, cstr);
            },
            [&](const Fortran::parser::ElseStmt *) { eval.isTarget = true; },
            [&](const Fortran::parser::EndIfStmt *) { eval.isTarget = true; },
            [&](const Fortran::parser::SelectRankStmt *) {
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::Switch, cstr);
            },
            [&](const Fortran::parser::SelectRankCaseStmt *) {
              eval.isTarget = true;
            },
            [&](const Fortran::parser::SelectTypeStmt *) {
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::Switch, cstr);
            },
            [&](const Fortran::parser::TypeGuardStmt *) {
              eval.isTarget = true;
            },
            [&](const Fortran::parser::WhereConstruct *) {
              // mark the WHERE as if it were a DO loop
              eval.isTarget = true;
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::Iterative, cstr);
            },
            [&](const Fortran::parser::WhereConstructStmt *) {
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::CondGoto, cstr);
            },
            [&](const Fortran::parser::MaskedElsewhereStmt *) {
              eval.isTarget = true;
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::CondGoto, cstr);
            },
            [&](const Fortran::parser::ElsewhereStmt *) {
              eval.isTarget = true;
            },
            [&](const Fortran::parser::EndWhereStmt *) {
              eval.isTarget = true;
            },
            [&](const Fortran::parser::ForallConstructStmt *) {
              eval.isTarget = true;
              eval.setCFG(Fortran::lower::AST::CFGAnnotation::Iterative, cstr);
            },
            [&](const Fortran::parser::EndForallStmt *) {
              eval.isTarget = true;
            },
            [](const auto *) { /* do nothing */ },
        },
        eval.u);
  }
}

/// Annotate the AST with CFG source decorations (see CFGAnnotation) and mark
/// potential branch targets
inline void
annotateFuncCFG(Fortran::lower::AST::FunctionLikeUnit &functionLikeUnit) {
  annotateEvalListCFG(functionLikeUnit.evals, nullptr);
}

llvm::StringRef evalName(Fortran::lower::AST::Evaluation &eval) {
  return std::visit(
      Fortran::common::visitors{
          [](const Fortran::lower::AST::CGJump) { return "CGJump"; },
          [](const auto *parseTreeNode) {
            assert(parseTreeNode && "nullptr node in AST ");
            return Fortran::parser::ParseTreeDumper::GetNodeName(
                *parseTreeNode);
          }},
      eval.u);
}

void dumpEvalList(
    llvm::raw_ostream &outputStream,
    Fortran::lower::AST::EvaluationCollection &evaluationCollection,
    int indent = 1) {
  static const std::string white{"                                      ++"};
  std::string indentString{white.substr(0, indent * 2)};
  for (Fortran::lower::AST::Evaluation &eval : evaluationCollection) {
    llvm::StringRef name{evalName(eval)};
    if (eval.isConstruct()) {
      outputStream << indentString << "<<" << name << ">>\n";
      dumpEvalList(outputStream, *eval.getConstructEvals(), indent + 1);
      outputStream << indentString << "<<End" << name << ">>\n";
    } else {
      outputStream << indentString << name << ": " << eval.pos.ToString()
                   << '\n';
    }
  }
}

void dumpFunctionLikeUnit(
    llvm::raw_ostream &outputStream,
    Fortran::lower::AST::FunctionLikeUnit &functionLikeUnit) {
  llvm::StringRef unitKind{};
  std::string name{};
  std::string header{};
  std::visit(
      Fortran::common::visitors{
          [&](const Fortran::parser::Statement<Fortran::parser::ProgramStmt>
                  *statement) {
            unitKind = "Program";
            name = statement->statement.v.ToString();
          },
          [&](const Fortran::parser::Statement<Fortran::parser::FunctionStmt>
                  *statement) {
            unitKind = "Function";
            name = std::get<Fortran::parser::Name>(statement->statement.t)
                       .ToString();
            header = statement->source.ToString();
          },
          [&](const Fortran::parser::Statement<Fortran::parser::SubroutineStmt>
                  *statement) {
            unitKind = "Subroutine";
            name = std::get<Fortran::parser::Name>(statement->statement.t)
                       .ToString();
            header = statement->source.ToString();
          },
          [&](const Fortran::parser::Statement<
              Fortran::parser::MpSubprogramStmt> *statement) {
            unitKind = "MpSubprogram";
            name = statement->statement.v.ToString();
            header = statement->source.ToString();
          },
          [&](auto *) {
            if (std::get_if<const Fortran::parser::Statement<
                    Fortran::parser::EndProgramStmt> *>(
                    &functionLikeUnit.funStmts.back())) {
              unitKind = "Program";
              name = "<anonymous>";
            } else {
              unitKind = ">>>>> Error - no program unit <<<<<";
            }
          },
      },
      functionLikeUnit.funStmts.front());
  outputStream << unitKind << ' ' << name;
  if (header.size())
    outputStream << ": " << header;
  outputStream << '\n';
  dumpEvalList(outputStream, functionLikeUnit.evals);
  outputStream << "End" << unitKind << ' ' << name << "\n\n";
}

} // namespace

Fortran::lower::AST::FunctionLikeUnit::FunctionLikeUnit(
    const Fortran::parser::MainProgram &func,
    const Fortran::lower::AST::ParentType &parent)
    : ProgramUnit{&func, parent} {
  auto &ps{std::get<
      std::optional<Fortran::parser::Statement<Fortran::parser::ProgramStmt>>>(
      func.t)};
  if (ps.has_value()) {
    const Fortran::parser::Statement<Fortran::parser::ProgramStmt> &statement{
        ps.value()};
    funStmts.push_back(&statement);
  }
  funStmts.push_back(
      &std::get<Fortran::parser::Statement<Fortran::parser::EndProgramStmt>>(
          func.t));
}

Fortran::lower::AST::FunctionLikeUnit::FunctionLikeUnit(
    const Fortran::parser::FunctionSubprogram &func,
    const Fortran::lower::AST::ParentType &parent)
    : ProgramUnit{&func, parent} {
  funStmts.push_back(
      &std::get<Fortran::parser::Statement<Fortran::parser::FunctionStmt>>(
          func.t));
  funStmts.push_back(
      &std::get<Fortran::parser::Statement<Fortran::parser::EndFunctionStmt>>(
          func.t));
}

Fortran::lower::AST::FunctionLikeUnit::FunctionLikeUnit(
    const Fortran::parser::SubroutineSubprogram &func,
    const Fortran::lower::AST::ParentType &parent)
    : ProgramUnit{&func, parent} {
  funStmts.push_back(
      &std::get<Fortran::parser::Statement<Fortran::parser::SubroutineStmt>>(
          func.t));
  funStmts.push_back(
      &std::get<Fortran::parser::Statement<Fortran::parser::EndSubroutineStmt>>(
          func.t));
}

Fortran::lower::AST::FunctionLikeUnit::FunctionLikeUnit(
    const Fortran::parser::SeparateModuleSubprogram &func,
    const Fortran::lower::AST::ParentType &parent)
    : ProgramUnit{&func, parent} {
  funStmts.push_back(
      &std::get<Fortran::parser::Statement<Fortran::parser::MpSubprogramStmt>>(
          func.t));
  funStmts.push_back(
      &std::get<
          Fortran::parser::Statement<Fortran::parser::EndMpSubprogramStmt>>(
          func.t));
}

Fortran::lower::AST::ModuleLikeUnit::ModuleLikeUnit(
    const Fortran::parser::Module &m,
    const Fortran::lower::AST::ParentType &parent)
    : ProgramUnit{&m, parent} {
  modStmts.push_back(
      &std::get<Fortran::parser::Statement<Fortran::parser::ModuleStmt>>(m.t));
  modStmts.push_back(
      &std::get<Fortran::parser::Statement<Fortran::parser::EndModuleStmt>>(
          m.t));
}

Fortran::lower::AST::ModuleLikeUnit::ModuleLikeUnit(
    const Fortran::parser::Submodule &m,
    const Fortran::lower::AST::ParentType &parent)
    : ProgramUnit{&m, parent} {
  modStmts.push_back(
      &std::get<Fortran::parser::Statement<Fortran::parser::SubmoduleStmt>>(
          m.t));
  modStmts.push_back(
      &std::get<Fortran::parser::Statement<Fortran::parser::EndSubmoduleStmt>>(
          m.t));
}

Fortran::lower::AST::BlockDataUnit::BlockDataUnit(
    const Fortran::parser::BlockData &bd,
    const Fortran::lower::AST::ParentType &parent)
    : ProgramUnit{&bd, parent} {}

Fortran::lower::AST::Program *
Fortran::lower::createAST(const Fortran::parser::Program &root) {
  ASTBuilder walker;
  Walk(root, walker);
  return walker.result();
}

void Fortran::lower::annotateControl(Fortran::lower::AST::Program &ast) {
  for (auto &unit : ast.getUnits()) {
    std::visit(Fortran::common::visitors{
                   [](Fortran::lower::AST::BlockDataUnit &) {},
                   [](Fortran::lower::AST::FunctionLikeUnit &func) {
                     annotateFuncCFG(func);
                     for (auto &statement : func.funcs) {
                       annotateFuncCFG(statement);
                     }
                   },
                   [](Fortran::lower::AST::ModuleLikeUnit &unit) {
                     for (auto &func : unit.funcs) {
                       annotateFuncCFG(func);
                     }
                   },
               },
               unit);
  }
}

/// Dump an AST.
void Fortran::lower::dumpAST(llvm::raw_ostream &outputStream,
                             Fortran::lower::AST::Program &ast) {
  for (auto &unit : ast.getUnits()) {
    std::visit(Fortran::common::visitors{
                   [&](Fortran::lower::AST::BlockDataUnit &) {
                     outputStream << "BlockData\nEndBlockData\n\n";
                   },
                   [&](Fortran::lower::AST::FunctionLikeUnit &func) {
                     dumpFunctionLikeUnit(outputStream, func);
                     for (auto &func : func.funcs) {
                       dumpFunctionLikeUnit(outputStream, func);
                     }
                   },
                   [&](Fortran::lower::AST::ModuleLikeUnit &unit) {
                     for (auto &func : unit.funcs) {
                       dumpFunctionLikeUnit(outputStream, func);
                     }
                   },
               },
               unit);
  }
}
