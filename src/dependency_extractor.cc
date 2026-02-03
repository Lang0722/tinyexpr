#include "spice_expr/visitor/dependency_extractor.h"

#include "spice_expr/ast/functions.h"
#include "spice_expr/ast/literals.h"
#include "spice_expr/ast/operations.h"
#include "spice_expr/ast/references.h"

namespace spice_expr {

ExprDependencies DependencyExtractor::extract(const ExprNode& node) {
  deps_ = ExprDependencies{};
  node.accept(*this);
  return deps_;
}

void DependencyExtractor::visit(const NumberLiteral&) {}

void DependencyExtractor::visit(const StringLiteral&) {}

void DependencyExtractor::visit(const ArrayLiteral& node) {
  for (const auto* elem : node.elements()) {
    elem->accept(*this);
  }
}

void DependencyExtractor::visit(const Identifier& node) {
  deps_.parameters.insert(node.name());
}

void DependencyExtractor::visit(const CircuitNodeRef& node) {
  deps_.nodeVoltages.insert(node.node1());
  if (node.is_differential()) {
    deps_.nodeVoltages.insert(node.node2());
  }
}

void DependencyExtractor::visit(const CircuitCurrentRef& node) {
  deps_.deviceCurrents.insert(node.device());
}

void DependencyExtractor::visit(const BinaryOp& node) {
  node.left()->accept(*this);
  node.right()->accept(*this);
}

void DependencyExtractor::visit(const UnaryOp& node) {
  node.operand()->accept(*this);
}

void DependencyExtractor::visit(const FunctionCall& node) {
  deps_.functions.insert(node.name());
  for (const auto* arg : node.arguments()) {
    arg->accept(*this);
  }
}

void DependencyExtractor::visit(const ArrayIndex& node) {
  node.array()->accept(*this);
  node.index()->accept(*this);
}

void DependencyExtractor::visit(const TernaryConditional& node) {
  node.condition()->accept(*this);
  node.true_expr()->accept(*this);
  node.false_expr()->accept(*this);
}

}  // namespace spice_expr
