//===-- lib/lower/PFTBuilder.cc -------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "flang/lower/PFTBuilder.h"
#include "flang/parser/dump-parse-tree.h"
#include "flang/parser/parse-tree-visitor.h"
#include <algorithm>
#include <cassert>
#include <utility>

namespace Fortran::lower {
namespace {

/// Helpers to unveil parser node inside parser::Statement<>,
/// parser::UnlabeledStatement, and common::Indirection<>
template <typename A>
struct RemoveIndirectionHelper {
  using Type = A;
  static constexpr const Type &unwrap(const A &a) { return a; }
};
template <typename A>
struct RemoveIndirectionHelper<common::Indirection<A>> {
  using Type = A;
  static constexpr const Type &unwrap(const common::Indirection<A> &a) {
    return a.value();
  }
};

template <typename A>
const auto &removeIndirection(const A &a) {
  return RemoveIndirectionHelper<A>::unwrap(a);
}

template <typename A>
struct UnwrapStmt {
  static constexpr bool isStmt{false};
};
template <typename A>
struct UnwrapStmt<parser::Statement<A>> {
  static constexpr bool isStmt{true};
  using Type = typename RemoveIndirectionHelper<A>::Type;
  constexpr UnwrapStmt(const parser::Statement<A> &a)
      : unwrapped{removeIndirection(a.statement)}, pos{a.source}, lab{a.label} {
  }
  const Type &unwrapped;
  parser::CharBlock pos;
  std::optional<parser::Label> lab;
};
template <typename A>
struct UnwrapStmt<parser::UnlabeledStatement<A>> {
  static constexpr bool isStmt{true};
  using Type = typename RemoveIndirectionHelper<A>::Type;
  constexpr UnwrapStmt(const parser::UnlabeledStatement<A> &a)
      : unwrapped{removeIndirection(a.statement)}, pos{a.source} {}
  const Type &unwrapped;
  parser::CharBlock pos;
  std::optional<parser::Label> lab;
};

/// The instantiation of a parse tree visitor (Pre and Post) is extremely
/// expensive in terms of compile and link time, so one goal here is to limit
/// the bridge to one such instantiation.
class PFTBuilder {
public:
  PFTBuilder() : pgm{new PFT::Program}, parents{*pgm.get()} {}

  /// Get the result
  std::unique_ptr<PFT::Program> result() { return std::move(pgm); }

  template <typename A>
  constexpr bool Pre(const A &a) {
    if constexpr (PFT::isFunctionLike<A>) {
      return enterFunc(a);
    } else if constexpr (PFT::isConstruct<A>) {
      return enterConstruct(a);
    }
    return true;
  }

  template <typename A>
  constexpr void Post(const A &a) {
    if constexpr (PFT::isFunctionLike<A>) {
      exitFunc();
    } else if constexpr (PFT::isConstruct<A>) {
      exitConstruct();
    } else if constexpr (UnwrapStmt<A>::isStmt) {
      using T = typename UnwrapStmt<A>::Type;
      // Node "a" being visited has one of the following types:
      // Statement<T>, Statement<Indirection<T>, UnlabeledStatement<T>,
      // or UnlabeledStatement<Indirection<T>>
      auto stmt{UnwrapStmt<A>(a)};
      if constexpr (PFT::isConstructStmts<T> || PFT::isOtherStmt<T>) {
        addEval(PFT::Evaluation{stmt.unwrapped, parents.back(), stmt.pos,
                                stmt.lab});
      } else if constexpr (std::is_same_v<T, parser::ActionStmt>) {
        addEval(makeEvalAction(stmt.unwrapped, stmt.pos, stmt.lab));
      }
    }
  }

  // Module like
  bool Pre(const parser::Module &node) { return enterModule(node); }
  bool Pre(const parser::Submodule &node) { return enterModule(node); }

  void Post(const parser::Module &) { exitModule(); }
  void Post(const parser::Submodule &) { exitModule(); }

  // Block data
  void Post(const parser::BlockData &node) {
    addUnit(PFT::BlockDataUnit{node, parents.back()});
  }

  // Get rid of production wrapper
  void Post(const parser::UnlabeledStatement<parser::ForallAssignmentStmt>
                &statement) {
    addEval(std::visit(
        [&](const auto &x) {
          return PFT::Evaluation{x, parents.back(), statement.source, {}};
        },
        statement.statement.u));
  }
  void Post(const parser::Statement<parser::ForallAssignmentStmt> &statement) {
    addEval(std::visit(
        [&](const auto &x) {
          return PFT::Evaluation{x, parents.back(), statement.source,
                                 statement.label};
        },
        statement.statement.u));
  }

private:
  // ActionStmt has a couple of non-conforming cases, which get handled
  // explicitly here.  The other cases use an Indirection, which we discard in
  // the PFT.
  PFT::Evaluation makeEvalAction(const parser::ActionStmt &statement,
                                 parser::CharBlock pos,
                                 std::optional<parser::Label> lab) {
    return std::visit(
        common::visitors{
            [&](const auto &x) {
              return PFT::Evaluation{removeIndirection(x), parents.back(), pos,
                                     lab};
            },
        },
        statement.u);
  }

  // When we enter a function-like structure, we want to build a new unit and
  // set the builder's cursors to point to it.
  template <typename A>
  bool enterFunc(const A &func) {
    auto &unit = addFunc(PFT::FunctionLikeUnit{func, parents.back()});
    funclist = &unit.funcs;
    pushEval(&unit.evals);
    parents.emplace_back(unit);
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
    auto &con = addEval(PFT::Evaluation{construct, parents.back()});
    con.subs.reset(new PFT::EvaluationCollection);
    pushEval(con.subs.get());
    parents.emplace_back(con);
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
    auto &unit = addUnit(PFT::ModuleLikeUnit{func, parents.back()});
    funclist = &unit.funcs;
    parents.emplace_back(unit);
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
  PFT::Evaluation &addEval(PFT::Evaluation &&eval) {
    assert(funclist && "not in a function");
    assert(evallist.size() > 0);
    evallist.back()->emplace_back(std::move(eval));
    return evallist.back()->back();
  }

  /// push a new list on the stack of Evaluation lists
  void pushEval(PFT::EvaluationCollection *eval) {
    assert(funclist && "not in a function");
    assert(eval && eval->empty() && "evaluation list isn't correct");
    evallist.emplace_back(eval);
  }

  /// pop the current list and return to the last Evaluation list
  void popEval() {
    assert(funclist && "not in a function");
    evallist.pop_back();
  }

  std::unique_ptr<PFT::Program> pgm;
  /// funclist points to FunctionLikeUnit::funcs list (resp.
  /// ModuleLikeUnit::funcs) when building a FunctionLikeUnit (resp.
  /// ModuleLikeUnit) to store internal procedures (resp. module procedures).
  /// Otherwise (e.g. when building the top level Program), it is null.
  std::list<PFT::FunctionLikeUnit> *funclist{nullptr};
  /// evallist is a stack of pointer to FunctionLikeUnit::evals (or
  /// Evaluation::subs) that are being build.
  std::vector<PFT::EvaluationCollection *> evallist;
  std::vector<PFT::ParentType> parents;
};

template <typename Label, typename A>
constexpr bool hasLabel(const A &stmt) {
  auto isLabel{
      [](const auto &v) { return std::holds_alternative<Label>(v.u); }};
  if constexpr (std::is_same_v<A, parser::ReadStmt> ||
                std::is_same_v<A, parser::WriteStmt>) {
    return std::any_of(std::begin(stmt.controls), std::end(stmt.controls),
                       isLabel);
  }
  if constexpr (std::is_same_v<A, parser::WaitStmt>) {
    return std::any_of(std::begin(stmt.v), std::end(stmt.v), isLabel);
  }
  if constexpr (std::is_same_v<Label, parser::ErrLabel>) {
    if constexpr (common::HasMember<
                      A, std::tuple<parser::OpenStmt, parser::CloseStmt,
                                    parser::BackspaceStmt, parser::EndfileStmt,
                                    parser::RewindStmt, parser::FlushStmt>>)
      return std::any_of(std::begin(stmt.v), std::end(stmt.v), isLabel);
    if constexpr (std::is_same_v<A, parser::InquireStmt>) {
      const auto &specifiers{std::get<std::list<parser::InquireSpec>>(stmt.u)};
      return std::any_of(std::begin(specifiers), std::end(specifiers), isLabel);
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
void altRet(PFT::Evaluation &evaluation, const parser::CallStmt &callStmt,
            PFT::Evaluation *cstr) {
  if (hasAltReturns(callStmt))
    evaluation.setCFG(PFT::CFGAnnotation::Switch, cstr);
}

/// \param cstr points to the current construct. It may be null at the top-level
/// of a FunctionLikeUnit.
void annotateEvalListCFG(PFT::EvaluationCollection &evaluationCollection,
                         PFT::Evaluation *cstr) {
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
    eval.visit(common::visitors{
        [&](const parser::CallStmt &statement) {
          altRet(eval, statement, cstr);
        },
        [&](const parser::CycleStmt &) {
          eval.setCFG(PFT::CFGAnnotation::Goto, cstr);
        },
        [&](const parser::ExitStmt &) {
          eval.setCFG(PFT::CFGAnnotation::Goto, cstr);
        },
        [&](const parser::FailImageStmt &) {
          eval.setCFG(PFT::CFGAnnotation::Terminate, cstr);
        },
        [&](const parser::GotoStmt &) {
          eval.setCFG(PFT::CFGAnnotation::Goto, cstr);
        },
        [&](const parser::IfStmt &) {
          eval.setCFG(PFT::CFGAnnotation::CondGoto, cstr);
        },
        [&](const parser::ReturnStmt &) {
          eval.setCFG(PFT::CFGAnnotation::Return, cstr);
        },
        [&](const parser::StopStmt &) {
          eval.setCFG(PFT::CFGAnnotation::Terminate, cstr);
        },
        [&](const parser::ArithmeticIfStmt &) {
          eval.setCFG(PFT::CFGAnnotation::Switch, cstr);
        },
        [&](const parser::AssignedGotoStmt &) {
          eval.setCFG(PFT::CFGAnnotation::IndGoto, cstr);
        },
        [&](const parser::ComputedGotoStmt &) {
          eval.setCFG(PFT::CFGAnnotation::Switch, cstr);
        },
        [&](const parser::WhereStmt &) {
          // fir.loop + fir.where around the next stmt
          eval.isTarget = true;
          eval.setCFG(PFT::CFGAnnotation::Iterative, cstr);
        },
        [&](const parser::ForallStmt &) {
          // fir.loop around the next stmt
          eval.isTarget = true;
          eval.setCFG(PFT::CFGAnnotation::Iterative, cstr);
        },
        [&](PFT::CGJump &) { eval.setCFG(PFT::CFGAnnotation::Goto, cstr); },
        [&](const parser::SelectCaseStmt &) {
          eval.setCFG(PFT::CFGAnnotation::Switch, cstr);
        },
        [&](const parser::NonLabelDoStmt &) {
          eval.isTarget = true;
          eval.setCFG(PFT::CFGAnnotation::Iterative, cstr);
        },
        [&](const parser::EndDoStmt &) {
          eval.isTarget = true;
          eval.setCFG(PFT::CFGAnnotation::Goto, cstr);
        },
        [&](const parser::IfThenStmt &) {
          eval.setCFG(PFT::CFGAnnotation::CondGoto, cstr);
        },
        [&](const parser::ElseIfStmt &) {
          eval.setCFG(PFT::CFGAnnotation::CondGoto, cstr);
        },
        [&](const parser::SelectRankStmt &) {
          eval.setCFG(PFT::CFGAnnotation::Switch, cstr);
        },
        [&](const parser::SelectTypeStmt &) {
          eval.setCFG(PFT::CFGAnnotation::Switch, cstr);
        },
        [&](const parser::WhereConstruct &) {
          // mark the WHERE as if it were a DO loop
          eval.isTarget = true;
          eval.setCFG(PFT::CFGAnnotation::Iterative, cstr);
        },
        [&](const parser::WhereConstructStmt &) {
          eval.setCFG(PFT::CFGAnnotation::CondGoto, cstr);
        },
        [&](const parser::MaskedElsewhereStmt &) {
          eval.isTarget = true;
          eval.setCFG(PFT::CFGAnnotation::CondGoto, cstr);
        },
        [&](const parser::ForallConstructStmt &) {
          eval.isTarget = true;
          eval.setCFG(PFT::CFGAnnotation::Iterative, cstr);
        },

        [&](const auto &stmt) {
          // Handle statements with similar impact on control flow
          using IoStmts = std::tuple<parser::BackspaceStmt, parser::CloseStmt,
                                     parser::EndfileStmt, parser::FlushStmt,
                                     parser::InquireStmt, parser::OpenStmt,
                                     parser::ReadStmt, parser::RewindStmt,
                                     parser::WaitStmt, parser::WriteStmt>;

          using TargetStmts =
              std::tuple<parser::EndAssociateStmt, parser::EndBlockStmt,
                         parser::CaseStmt, parser::EndSelectStmt,
                         parser::EndChangeTeamStmt, parser::EndCriticalStmt,
                         parser::ElseStmt, parser::EndIfStmt,
                         parser::SelectRankCaseStmt, parser::TypeGuardStmt,
                         parser::ElsewhereStmt, parser::EndWhereStmt,
                         parser::EndForallStmt>;

          using DoNothingConstructStmts =
              std::tuple<parser::BlockStmt, parser::AssociateStmt,
                         parser::CriticalStmt, parser::ChangeTeamStmt>;

          using A = std::decay_t<decltype(stmt)>;
          if constexpr (common::HasMember<A, IoStmts>) {
            if (hasLabel<parser::ErrLabel>(stmt) ||
                hasLabel<parser::EorLabel>(stmt) ||
                hasLabel<parser::EndLabel>(stmt))
              eval.setCFG(PFT::CFGAnnotation::IoSwitch, cstr);
          } else if constexpr (common::HasMember<A, TargetStmts>) {
            eval.isTarget = true;
          } else if constexpr (common::HasMember<A, DoNothingConstructStmts>) {
            // Explicitly do nothing for these construct statements
          } else {
            static_assert(!PFT::isConstructStmts<A>,
                          "All ConstructStmts impact on the control flow "
                          "should be explicitly handled");
          }
          /* else do nothing */
        },
    });
  }
}

/// Annotate the PFT with CFG source decorations (see CFGAnnotation) and mark
/// potential branch targets
inline void annotateFuncCFG(PFT::FunctionLikeUnit &functionLikeUnit) {
  annotateEvalListCFG(functionLikeUnit.evals, nullptr);
}

llvm::StringRef evalName(PFT::Evaluation &eval) {
  return eval.visit(common::visitors{
      [](const PFT::CGJump) { return "CGJump"; },
      [](const auto &parseTreeNode) {
        return parser::ParseTreeDumper::GetNodeName(parseTreeNode);
      },
  });
}

void dumpEvalList(llvm::raw_ostream &outputStream,
                  PFT::EvaluationCollection &evaluationCollection,
                  int indent = 1) {
  static const std::string white{"                                      ++"};
  std::string indentString{white.substr(0, indent * 2)};
  for (PFT::Evaluation &eval : evaluationCollection) {
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
                          PFT::FunctionLikeUnit &functionLikeUnit) {
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

PFT::FunctionLikeUnit::FunctionLikeUnit(const parser::MainProgram &func,
                                        const PFT::ParentType &parent)
    : ProgramUnit{func, parent} {
  auto &ps{
      std::get<std::optional<parser::Statement<parser::ProgramStmt>>>(func.t)};
  if (ps.has_value()) {
    const parser::Statement<parser::ProgramStmt> &statement{ps.value()};
    funStmts.push_back(&statement);
  }
  funStmts.push_back(
      &std::get<parser::Statement<parser::EndProgramStmt>>(func.t));
}

PFT::FunctionLikeUnit::FunctionLikeUnit(const parser::FunctionSubprogram &func,
                                        const PFT::ParentType &parent)
    : ProgramUnit{func, parent} {
  funStmts.push_back(
      &std::get<parser::Statement<parser::FunctionStmt>>(func.t));
  funStmts.push_back(
      &std::get<parser::Statement<parser::EndFunctionStmt>>(func.t));
}

PFT::FunctionLikeUnit::FunctionLikeUnit(
    const parser::SubroutineSubprogram &func, const PFT::ParentType &parent)
    : ProgramUnit{func, parent} {
  funStmts.push_back(
      &std::get<parser::Statement<parser::SubroutineStmt>>(func.t));
  funStmts.push_back(
      &std::get<parser::Statement<parser::EndSubroutineStmt>>(func.t));
}

PFT::FunctionLikeUnit::FunctionLikeUnit(
    const parser::SeparateModuleSubprogram &func, const PFT::ParentType &parent)
    : ProgramUnit{func, parent} {
  funStmts.push_back(
      &std::get<parser::Statement<parser::MpSubprogramStmt>>(func.t));
  funStmts.push_back(
      &std::get<parser::Statement<parser::EndMpSubprogramStmt>>(func.t));
}

PFT::ModuleLikeUnit::ModuleLikeUnit(const parser::Module &m,
                                    const PFT::ParentType &parent)
    : ProgramUnit{m, parent} {
  modStmts.push_back(&std::get<parser::Statement<parser::ModuleStmt>>(m.t));
  modStmts.push_back(&std::get<parser::Statement<parser::EndModuleStmt>>(m.t));
}

PFT::ModuleLikeUnit::ModuleLikeUnit(const parser::Submodule &m,
                                    const PFT::ParentType &parent)
    : ProgramUnit{m, parent} {
  modStmts.push_back(&std::get<parser::Statement<parser::SubmoduleStmt>>(m.t));
  modStmts.push_back(
      &std::get<parser::Statement<parser::EndSubmoduleStmt>>(m.t));
}

PFT::BlockDataUnit::BlockDataUnit(const parser::BlockData &bd,
                                  const PFT::ParentType &parent)
    : ProgramUnit{bd, parent} {}

std::unique_ptr<PFT::Program> createPFT(const parser::Program &root) {
  PFTBuilder walker;
  Walk(root, walker);
  return walker.result();
}

void annotateControl(PFT::Program &pft) {
  for (auto &unit : pft.getUnits()) {
    std::visit(common::visitors{
                   [](PFT::BlockDataUnit &) {},
                   [](PFT::FunctionLikeUnit &func) {
                     annotateFuncCFG(func);
                     for (auto &statement : func.funcs) {
                       annotateFuncCFG(statement);
                     }
                   },
                   [](PFT::ModuleLikeUnit &unit) {
                     for (auto &func : unit.funcs) {
                       annotateFuncCFG(func);
                     }
                   },
               },
               unit);
  }
}

/// Dump an PFT.
void dumpPFT(llvm::raw_ostream &outputStream, PFT::Program &pft) {
  for (auto &unit : pft.getUnits()) {
    std::visit(common::visitors{
                   [&](PFT::BlockDataUnit &) {
                     outputStream << "BlockData\nEndBlockData\n\n";
                   },
                   [&](PFT::FunctionLikeUnit &func) {
                     dumpFunctionLikeUnit(outputStream, func);
                     for (auto &func : func.funcs) {
                       dumpFunctionLikeUnit(outputStream, func);
                     }
                   },
                   [&](PFT::ModuleLikeUnit &unit) {
                     for (auto &func : unit.funcs) {
                       dumpFunctionLikeUnit(outputStream, func);
                     }
                   },
               },
               unit);
  }
}

} // namespace Fortran::lower