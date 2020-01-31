//===-- lib/lower/ASTBuilder.cc -------------------------------------------===//
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

namespace Fortran::lower {
namespace {

/// The instantiation of a parse tree visitor (Pre and Post) is extremely
/// expensive in terms of compile and link time, so one goal here is to limit
/// the bridge to one such instantiation.
class ASTBuilder {
public:
  ASTBuilder() : pgm{new AST::Program}, parents{pgm.get()} {}

  /// Get the result
  std::unique_ptr<AST::Program> result() { return std::move(pgm); }

  template <typename A>
  constexpr bool Pre(const A &) {
    return true;
  }
  template <typename A>
  constexpr void Post(const A &) {}

  // Module like

  bool Pre(const parser::Module &node) { return enterModule(node); }
  bool Pre(const parser::Submodule &node) { return enterModule(node); }

  void Post(const parser::Module &) { exitModule(); }
  void Post(const parser::Submodule &) { exitModule(); }

  // Function like

  bool Pre(const parser::MainProgram &node) { return enterFunc(node); }
  bool Pre(const parser::FunctionSubprogram &node) { return enterFunc(node); }
  bool Pre(const parser::SubroutineSubprogram &node) { return enterFunc(node); }
  bool Pre(const parser::SeparateModuleSubprogram &node) {
    return enterFunc(node);
  }

  void Post(const parser::MainProgram &) { exitFunc(); }
  void Post(const parser::FunctionSubprogram &) { exitFunc(); }
  void Post(const parser::SubroutineSubprogram &) { exitFunc(); }
  void Post(const parser::SeparateModuleSubprogram &) { exitFunc(); }

  // Block data

  void Post(const parser::BlockData &node) {
    addUnit(AST::BlockDataUnit{node, parents.back()});
  }

  //
  // Action statements
  //

  void Post(const parser::Statement<parser::ActionStmt> &s) {
    addEval(makeEvalAction(s));
  }
  void Post(const parser::UnlabeledStatement<parser::ActionStmt> &s) {
    addEval(makeEvalAction(s));
  }

  //
  // Non-executable statements
  //

  void Post(const parser::Statement<common::Indirection<parser::FormatStmt>>
                &statement) {
    addEval(makeEvalIndirect(statement));
  }
  void Post(const parser::Statement<common::Indirection<parser::EntryStmt>>
                &statement) {
    addEval(makeEvalIndirect(statement));
  }
  void Post(const parser::Statement<common::Indirection<parser::DataStmt>>
                &statement) {
    addEval(makeEvalIndirect(statement));
  }
  void Post(const parser::Statement<common::Indirection<parser::NamelistStmt>>
                &statement) {
    addEval(makeEvalIndirect(statement));
  }

  //
  // Construct statements
  //

  void Post(const parser::Statement<parser::AssociateStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::EndAssociateStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::BlockStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::EndBlockStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::SelectCaseStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::CaseStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::EndSelectStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::ChangeTeamStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::EndChangeTeamStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::CriticalStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::EndCriticalStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::NonLabelDoStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::EndDoStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::IfThenStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::ElseIfStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::ElseStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::EndIfStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::SelectRankStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::SelectRankCaseStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::SelectTypeStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::TypeGuardStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::WhereConstructStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::MaskedElsewhereStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::ElsewhereStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::EndWhereStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::ForallConstructStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  void Post(const parser::Statement<parser::EndForallStmt> &statement) {
    addEval(makeEvalDirect(statement));
  }
  // Get rid of production wrapper
  void Post(const parser::UnlabeledStatement<parser::ForallAssignmentStmt>
                &statement) {
    addEval(std::visit(
        [&](const auto &x) {
          return AST::Evaluation{x, parents.back(), statement.source, {}};
        },
        statement.statement.u));
  }
  void Post(const parser::Statement<parser::ForallAssignmentStmt> &statement) {
    addEval(std::visit(
        [&](const auto &x) {
          return AST::Evaluation{x, parents.back(), statement.source,
                                 statement.label};
        },
        statement.statement.u));
  }

  //
  // Constructs (enter and exit)
  //

  bool Pre(const parser::AssociateConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const parser::BlockConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const parser::CaseConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const parser::ChangeTeamConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const parser::CriticalConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const parser::DoConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const parser::IfConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const parser::SelectRankConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const parser::SelectTypeConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const parser::WhereConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const parser::ForallConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const parser::CompilerDirective &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const parser::OpenMPConstruct &construct) {
    return enterConstruct(construct);
  }
  bool Pre(const parser::OmpEndLoopDirective &construct) {
    return enterConstruct(construct);
  }

  void Post(const parser::AssociateConstruct &) { exitConstruct(); }
  void Post(const parser::BlockConstruct &) { exitConstruct(); }
  void Post(const parser::CaseConstruct &) { exitConstruct(); }
  void Post(const parser::ChangeTeamConstruct &) { exitConstruct(); }
  void Post(const parser::CriticalConstruct &) { exitConstruct(); }
  void Post(const parser::DoConstruct &) { exitConstruct(); }
  void Post(const parser::IfConstruct &) { exitConstruct(); }
  void Post(const parser::SelectRankConstruct &) { exitConstruct(); }
  void Post(const parser::SelectTypeConstruct &) { exitConstruct(); }
  void Post(const parser::WhereConstruct &) { exitConstruct(); }
  void Post(const parser::ForallConstruct &) { exitConstruct(); }
  void Post(const parser::CompilerDirective &) { exitConstruct(); }
  void Post(const parser::OpenMPConstruct &) { exitConstruct(); }
  void Post(const parser::OmpEndLoopDirective &) { exitConstruct(); }

private:
  // ActionStmt has a couple of non-conforming cases, which get handled
  // explicitly here.  The other cases use an Indirection, which we discard in
  // the AST.
  AST::Evaluation
  makeEvalAction(const parser::Statement<parser::ActionStmt> &statement) {
    return std::visit(
        common::visitors{
            [&](const parser::ContinueStmt &x) {
              return AST::Evaluation{x, parents.back(), statement.source,
                                     statement.label};
            },
            [&](const parser::FailImageStmt &x) {
              return AST::Evaluation{x, parents.back(), statement.source,
                                     statement.label};
            },
            [&](const auto &x) {
              return AST::Evaluation{x.value(), parents.back(),
                                     statement.source, statement.label};
            },
        },
        statement.statement.u);
  }
  AST::Evaluation makeEvalAction(
      const parser::UnlabeledStatement<parser::ActionStmt> &statement) {
    return std::visit(
        common::visitors{
            [&](const parser::ContinueStmt &x) {
              return AST::Evaluation{x, parents.back(), statement.source, {}};
            },
            [&](const parser::FailImageStmt &x) {
              return AST::Evaluation{x, parents.back(), statement.source, {}};
            },
            [&](const auto &x) {
              return AST::Evaluation{
                  x.value(), parents.back(), statement.source, {}};
            },
        },
        statement.statement.u);
  }

  template <typename A>
  AST::Evaluation
  makeEvalIndirect(const parser::Statement<common::Indirection<A>> &statement) {
    return AST::Evaluation{statement.statement.value(), parents.back(),
                           statement.source, statement.label};
  }

  template <typename A>
  AST::Evaluation makeEvalDirect(const parser::Statement<A> &statement) {
    return AST::Evaluation{statement.statement, parents.back(),
                           statement.source, statement.label};
  }

  // When we enter a function-like structure, we want to build a new unit and
  // set the builder's cursors to point to it.
  template <typename A>
  bool enterFunc(const A &func) {
    auto &unit = addFunc(AST::FunctionLikeUnit{func, parents.back()});
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
    auto &con = addEval(AST::Evaluation{construct, parents.back()});
    con.subs.reset(new AST::EvaluationCollection);
    pushEval(con.subs.get());
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
    auto &unit = addUnit(AST::ModuleLikeUnit{func, parents.back()});
    funclist = &unit.funcs;
    parents.emplace_back(&unit);
    return true;
  }

  void exitModule() {
    funclist = nullptr;
    parents.pop_back();
  }

  template <typename A>
  A &addUnit(A &&unit) {
    pgm->getUnits().emplace_back(std::move(unit));
    return std::get<A>(pgm->getUnits().back());
  }

  template <typename A>
  A &addFunc(A &&func) {
    if (funclist) {
      funclist->emplace_back(std::move(func));
      return funclist->back();
    }
    return addUnit(std::move(func));
  }

  /// move the Evaluation to the end of the current list
  AST::Evaluation &addEval(AST::Evaluation &&eval) {
    assert(funclist && "not in a function");
    assert(evallist.size() > 0);
    evallist.back()->emplace_back(std::move(eval));
    return evallist.back()->back();
  }

  /// push a new list on the stack of Evaluation lists
  void pushEval(AST::EvaluationCollection *eval) {
    assert(funclist && "not in a function");
    assert(eval && eval->empty() && "evaluation list isn't correct");
    evallist.emplace_back(eval);
  }

  /// pop the current list and return to the last Evaluation list
  void popEval() {
    assert(funclist && "not in a function");
    evallist.pop_back();
  }

  std::unique_ptr<AST::Program> pgm;
  /// funclist points to FunctionLikeUnit::funcs list (resp.
  /// ModuleLikeUnit::funcs) when building a FunctionLikeUnit (resp.
  /// ModuleLikeUnit) to store internal procedures (resp. module procedures).
  /// Otherwise (e.g. when building the top level Program), it is null.
  std::list<AST::FunctionLikeUnit> *funclist{nullptr};
  /// evallist is a stack of pointer to FunctionLikeUnit::evals (or
  /// Evaluation::subs) that are being build.
  std::vector<AST::EvaluationCollection *> evallist;
  std::vector<AST::ParentType> parents;
};

template <typename A>
constexpr bool hasErrLabel(const A &stmt) {
  auto isError{[](const auto &v) {
    return std::holds_alternative<parser::ErrLabel>(v.u);
  }};
  if constexpr (std::is_same_v<A, parser::ReadStmt> ||
                std::is_same_v<A, parser::WriteStmt>) {
    return std::any_of(std::begin(stmt.controls), std::end(stmt.controls),
                       isError);
  }
  if constexpr (std::is_same_v<A, parser::WaitStmt> ||
                std::is_same_v<A, parser::OpenStmt> ||
                std::is_same_v<A, parser::CloseStmt> ||
                std::is_same_v<A, parser::BackspaceStmt> ||
                std::is_same_v<A, parser::EndfileStmt> ||
                std::is_same_v<A, parser::RewindStmt> ||
                std::is_same_v<A, parser::FlushStmt>) {
    return std::any_of(std::begin(stmt.v), std::end(stmt.v), isError);
  }
  if constexpr (std::is_same_v<A, parser::InquireStmt>) {
    const auto &specifiers{std::get<std::list<parser::InquireSpec>>(stmt.u)};
    return std::any_of(std::begin(specifiers), std::end(specifiers), isError);
  }
  return false;
}

template <typename A>
constexpr bool hasEorLabel(const A &stmt) {
  if constexpr (std::is_same_v<A, parser::ReadStmt> ||
                std::is_same_v<A, parser::WriteStmt>) {
    for (const auto &control : stmt.controls) {
      if (std::holds_alternative<parser::EorLabel>(control.u))
        return true;
    }
  }
  if constexpr (std::is_same_v<A, parser::WaitStmt>) {
    for (const auto &waitSpec : stmt.v) {
      if (std::holds_alternative<parser::EorLabel>(waitSpec.u))
        return true;
    }
  }
  return false;
}

template <typename A>
constexpr bool hasEndLabel(const A &stmt) {
  if constexpr (std::is_same_v<A, parser::ReadStmt> ||
                std::is_same_v<A, parser::WriteStmt>) {
    for (const auto &control : stmt.controls) {
      if (std::holds_alternative<parser::EndLabel>(control.u))
        return true;
    }
  }
  if constexpr (std::is_same_v<A, parser::WaitStmt>) {
    for (const auto &waitSpec : stmt.v) {
      if (std::holds_alternative<parser::EndLabel>(waitSpec.u))
        return true;
    }
  }
  return false;
}

bool hasAltReturns(const parser::CallStmt &callStmt) {
  const auto &args{std::get<std::list<parser::ActualArgSpec>>(callStmt.v.t)};
  for (const auto &arg : args) {
    const auto &actual{std::get<parser::ActualArg>(arg.t)};
    if (std::holds_alternative<parser::AltReturnSpec>(actual.u))
      return true;
  }
  return false;
}

/// Determine if `callStmt` has alternate returns and if so set `e` to be the
/// origin of a switch-like control flow
///
/// \param cstr points to the current construct. It may be null at the top-level
/// of a FunctionLikeUnit.
void altRet(AST::Evaluation &evaluation, const parser::CallStmt *callStmt,
            AST::Evaluation *cstr) {
  if (hasAltReturns(*callStmt))
    evaluation.setCFG(AST::CFGAnnotation::Switch, cstr);
}

template <typename A>
void ioLabel(AST::Evaluation &evaluation, const A *statement,
             AST::Evaluation *cstr) {
  if (hasErrLabel(*statement) || hasEorLabel(*statement) ||
      hasEndLabel(*statement))
    evaluation.setCFG(AST::CFGAnnotation::IoSwitch, cstr);
}

/// \param cstr points to the current construct. It may be null at the top-level
/// of a FunctionLikeUnit.
void annotateEvalListCFG(AST::EvaluationCollection &evaluationCollection,
                         AST::Evaluation *cstr) {
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
        common::visitors{
            [&](const parser::BackspaceStmt *statement) {
              ioLabel(eval, statement, cstr);
            },
            [&](const parser::CallStmt *statement) {
              altRet(eval, statement, cstr);
            },
            [&](const parser::CloseStmt *statement) {
              ioLabel(eval, statement, cstr);
            },
            [&](const parser::CycleStmt *) {
              eval.setCFG(AST::CFGAnnotation::Goto, cstr);
            },
            [&](const parser::EndfileStmt *statement) {
              ioLabel(eval, statement, cstr);
            },
            [&](const parser::ExitStmt *) {
              eval.setCFG(AST::CFGAnnotation::Goto, cstr);
            },
            [&](const parser::FailImageStmt *) {
              eval.setCFG(AST::CFGAnnotation::Terminate, cstr);
            },
            [&](const parser::FlushStmt *statement) {
              ioLabel(eval, statement, cstr);
            },
            [&](const parser::GotoStmt *) {
              eval.setCFG(AST::CFGAnnotation::Goto, cstr);
            },
            [&](const parser::IfStmt *) {
              eval.setCFG(AST::CFGAnnotation::CondGoto, cstr);
            },
            [&](const parser::InquireStmt *statement) {
              ioLabel(eval, statement, cstr);
            },
            [&](const parser::OpenStmt *statement) {
              ioLabel(eval, statement, cstr);
            },
            [&](const parser::ReadStmt *statement) {
              ioLabel(eval, statement, cstr);
            },
            [&](const parser::ReturnStmt *) {
              eval.setCFG(AST::CFGAnnotation::Return, cstr);
            },
            [&](const parser::RewindStmt *statement) {
              ioLabel(eval, statement, cstr);
            },
            [&](const parser::StopStmt *) {
              eval.setCFG(AST::CFGAnnotation::Terminate, cstr);
            },
            [&](const parser::WaitStmt *statement) {
              ioLabel(eval, statement, cstr);
            },
            [&](const parser::WriteStmt *statement) {
              ioLabel(eval, statement, cstr);
            },
            [&](const parser::ArithmeticIfStmt *) {
              eval.setCFG(AST::CFGAnnotation::Switch, cstr);
            },
            [&](const parser::AssignedGotoStmt *) {
              eval.setCFG(AST::CFGAnnotation::IndGoto, cstr);
            },
            [&](const parser::ComputedGotoStmt *) {
              eval.setCFG(AST::CFGAnnotation::Switch, cstr);
            },
            [&](const parser::WhereStmt *) {
              // fir.loop + fir.where around the next stmt
              eval.isTarget = true;
              eval.setCFG(AST::CFGAnnotation::Iterative, cstr);
            },
            [&](const parser::ForallStmt *) {
              // fir.loop around the next stmt
              eval.isTarget = true;
              eval.setCFG(AST::CFGAnnotation::Iterative, cstr);
            },
            [&](AST::CGJump &) { eval.setCFG(AST::CFGAnnotation::Goto, cstr); },
            [&](const parser::EndAssociateStmt *) { eval.isTarget = true; },
            [&](const parser::EndBlockStmt *) { eval.isTarget = true; },
            [&](const parser::SelectCaseStmt *) {
              eval.setCFG(AST::CFGAnnotation::Switch, cstr);
            },
            [&](const parser::CaseStmt *) { eval.isTarget = true; },
            [&](const parser::EndSelectStmt *) { eval.isTarget = true; },
            [&](const parser::EndChangeTeamStmt *) { eval.isTarget = true; },
            [&](const parser::EndCriticalStmt *) { eval.isTarget = true; },
            [&](const parser::NonLabelDoStmt *) {
              eval.isTarget = true;
              eval.setCFG(AST::CFGAnnotation::Iterative, cstr);
            },
            [&](const parser::EndDoStmt *) {
              eval.isTarget = true;
              eval.setCFG(AST::CFGAnnotation::Goto, cstr);
            },
            [&](const parser::IfThenStmt *) {
              eval.setCFG(AST::CFGAnnotation::CondGoto, cstr);
            },
            [&](const parser::ElseIfStmt *) {
              eval.setCFG(AST::CFGAnnotation::CondGoto, cstr);
            },
            [&](const parser::ElseStmt *) { eval.isTarget = true; },
            [&](const parser::EndIfStmt *) { eval.isTarget = true; },
            [&](const parser::SelectRankStmt *) {
              eval.setCFG(AST::CFGAnnotation::Switch, cstr);
            },
            [&](const parser::SelectRankCaseStmt *) { eval.isTarget = true; },
            [&](const parser::SelectTypeStmt *) {
              eval.setCFG(AST::CFGAnnotation::Switch, cstr);
            },
            [&](const parser::TypeGuardStmt *) { eval.isTarget = true; },
            [&](const parser::WhereConstruct *) {
              // mark the WHERE as if it were a DO loop
              eval.isTarget = true;
              eval.setCFG(AST::CFGAnnotation::Iterative, cstr);
            },
            [&](const parser::WhereConstructStmt *) {
              eval.setCFG(AST::CFGAnnotation::CondGoto, cstr);
            },
            [&](const parser::MaskedElsewhereStmt *) {
              eval.isTarget = true;
              eval.setCFG(AST::CFGAnnotation::CondGoto, cstr);
            },
            [&](const parser::ElsewhereStmt *) { eval.isTarget = true; },
            [&](const parser::EndWhereStmt *) { eval.isTarget = true; },
            [&](const parser::ForallConstructStmt *) {
              eval.isTarget = true;
              eval.setCFG(AST::CFGAnnotation::Iterative, cstr);
            },
            [&](const parser::EndForallStmt *) { eval.isTarget = true; },
            [](const auto *) { /* do nothing */ },
        },
        eval.u);
  }
}

/// Annotate the AST with CFG source decorations (see CFGAnnotation) and mark
/// potential branch targets
inline void annotateFuncCFG(AST::FunctionLikeUnit &functionLikeUnit) {
  annotateEvalListCFG(functionLikeUnit.evals, nullptr);
}

llvm::StringRef evalName(AST::Evaluation &eval) {
  return std::visit(
      common::visitors{[](const AST::CGJump) { return "CGJump"; },
                       [](const auto *parseTreeNode) {
                         assert(parseTreeNode && "nullptr node in AST ");
                         return parser::ParseTreeDumper::GetNodeName(
                             *parseTreeNode);
                       }},
      eval.u);
}

void dumpEvalList(llvm::raw_ostream &outputStream,
                  AST::EvaluationCollection &evaluationCollection,
                  int indent = 1) {
  static const std::string white{"                                      ++"};
  std::string indentString{white.substr(0, indent * 2)};
  for (AST::Evaluation &eval : evaluationCollection) {
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

void dumpFunctionLikeUnit(llvm::raw_ostream &outputStream,
                          AST::FunctionLikeUnit &functionLikeUnit) {
  llvm::StringRef unitKind{};
  std::string name{};
  std::string header{};
  std::visit(
      common::visitors{
          [&](const parser::Statement<parser::ProgramStmt> *statement) {
            unitKind = "Program";
            name = statement->statement.v.ToString();
          },
          [&](const parser::Statement<parser::FunctionStmt> *statement) {
            unitKind = "Function";
            name = std::get<parser::Name>(statement->statement.t).ToString();
            header = statement->source.ToString();
          },
          [&](const parser::Statement<parser::SubroutineStmt> *statement) {
            unitKind = "Subroutine";
            name = std::get<parser::Name>(statement->statement.t).ToString();
            header = statement->source.ToString();
          },
          [&](const parser::Statement<parser::MpSubprogramStmt> *statement) {
            unitKind = "MpSubprogram";
            name = statement->statement.v.ToString();
            header = statement->source.ToString();
          },
          [&](auto *) {
            if (std::get_if<const parser::Statement<parser::EndProgramStmt> *>(
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

AST::FunctionLikeUnit::FunctionLikeUnit(const parser::MainProgram &func,
                                        const AST::ParentType &parent)
    : ProgramUnit{&func, parent} {
  auto &ps{
      std::get<std::optional<parser::Statement<parser::ProgramStmt>>>(func.t)};
  if (ps.has_value()) {
    const parser::Statement<parser::ProgramStmt> &statement{ps.value()};
    funStmts.push_back(&statement);
  }
  funStmts.push_back(
      &std::get<parser::Statement<parser::EndProgramStmt>>(func.t));
}

AST::FunctionLikeUnit::FunctionLikeUnit(const parser::FunctionSubprogram &func,
                                        const AST::ParentType &parent)
    : ProgramUnit{&func, parent} {
  funStmts.push_back(
      &std::get<parser::Statement<parser::FunctionStmt>>(func.t));
  funStmts.push_back(
      &std::get<parser::Statement<parser::EndFunctionStmt>>(func.t));
}

AST::FunctionLikeUnit::FunctionLikeUnit(
    const parser::SubroutineSubprogram &func, const AST::ParentType &parent)
    : ProgramUnit{&func, parent} {
  funStmts.push_back(
      &std::get<parser::Statement<parser::SubroutineStmt>>(func.t));
  funStmts.push_back(
      &std::get<parser::Statement<parser::EndSubroutineStmt>>(func.t));
}

AST::FunctionLikeUnit::FunctionLikeUnit(
    const parser::SeparateModuleSubprogram &func, const AST::ParentType &parent)
    : ProgramUnit{&func, parent} {
  funStmts.push_back(
      &std::get<parser::Statement<parser::MpSubprogramStmt>>(func.t));
  funStmts.push_back(
      &std::get<parser::Statement<parser::EndMpSubprogramStmt>>(func.t));
}

AST::ModuleLikeUnit::ModuleLikeUnit(const parser::Module &m,
                                    const AST::ParentType &parent)
    : ProgramUnit{&m, parent} {
  modStmts.push_back(&std::get<parser::Statement<parser::ModuleStmt>>(m.t));
  modStmts.push_back(&std::get<parser::Statement<parser::EndModuleStmt>>(m.t));
}

AST::ModuleLikeUnit::ModuleLikeUnit(const parser::Submodule &m,
                                    const AST::ParentType &parent)
    : ProgramUnit{&m, parent} {
  modStmts.push_back(&std::get<parser::Statement<parser::SubmoduleStmt>>(m.t));
  modStmts.push_back(
      &std::get<parser::Statement<parser::EndSubmoduleStmt>>(m.t));
}

AST::BlockDataUnit::BlockDataUnit(const parser::BlockData &bd,
                                  const AST::ParentType &parent)
    : ProgramUnit{&bd, parent} {}

std::unique_ptr<AST::Program> createAST(const parser::Program &root) {
  ASTBuilder walker;
  Walk(root, walker);
  return walker.result();
}

void annotateControl(AST::Program &ast) {
  for (auto &unit : ast.getUnits()) {
    std::visit(common::visitors{
                   [](AST::BlockDataUnit &) {},
                   [](AST::FunctionLikeUnit &func) {
                     annotateFuncCFG(func);
                     for (auto &statement : func.funcs) {
                       annotateFuncCFG(statement);
                     }
                   },
                   [](AST::ModuleLikeUnit &unit) {
                     for (auto &func : unit.funcs) {
                       annotateFuncCFG(func);
                     }
                   },
               },
               unit);
  }
}

/// Dump an AST.
void dumpAST(llvm::raw_ostream &outputStream, AST::Program &ast) {
  for (auto &unit : ast.getUnits()) {
    std::visit(common::visitors{
                   [&](AST::BlockDataUnit &) {
                     outputStream << "BlockData\nEndBlockData\n\n";
                   },
                   [&](AST::FunctionLikeUnit &func) {
                     dumpFunctionLikeUnit(outputStream, func);
                     for (auto &func : func.funcs) {
                       dumpFunctionLikeUnit(outputStream, func);
                     }
                   },
                   [&](AST::ModuleLikeUnit &unit) {
                     for (auto &func : unit.funcs) {
                       dumpFunctionLikeUnit(outputStream, func);
                     }
                   },
               },
               unit);
  }
}

} // namespace Fortran::lower
