#ifndef SPICE_EXPR_VISITOR_DEPENDENCY_EXTRACTOR_H
#define SPICE_EXPR_VISITOR_DEPENDENCY_EXTRACTOR_H

#include <set>
#include <string>

#include "spice_expr/visitor/visitor.h"

namespace spice_expr {

class ExprNode;

struct ExprDependencies {
  std::set<std::string> parameters;
  std::set<std::string> nodeVoltages;
  std::set<std::string> deviceCurrents;
  std::set<std::string> functions;
};

class DependencyExtractor : public ConstExprVisitor {
 public:
  DependencyExtractor() = default;

  ExprDependencies extract(const ExprNode& node);

  void visit(const NumberLiteral& node) override;
  void visit(const StringLiteral& node) override;
  void visit(const ArrayLiteral& node) override;
  void visit(const Identifier& node) override;
  void visit(const CircuitNodeRef& node) override;
  void visit(const CircuitCurrentRef& node) override;
  void visit(const BinaryOp& node) override;
  void visit(const UnaryOp& node) override;
  void visit(const FunctionCall& node) override;
  void visit(const ArrayIndex& node) override;
  void visit(const TernaryConditional& node) override;

 private:
  ExprDependencies deps_;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_VISITOR_DEPENDENCY_EXTRACTOR_H
