#include "spice_expr/ast/expression_builder.h"

#include <stdexcept>

#include "spice_expr/ast/arena.h"
#include "spice_expr/ast/functions.h"
#include "spice_expr/ast/literals.h"
#include "spice_expr/ast/operations.h"
#include "spice_expr/ast/references.h"

namespace spice_expr {

ExpressionBuilder::ExpressionBuilder(ExprArena& arena) : arena_(arena) {}

// Private helpers

ExprNode* ExpressionBuilder::pop() {
  if (stack_.empty()) {
    throw std::runtime_error("ExpressionBuilder: stack underflow");
  }
  ExprNode* node = stack_.back();
  stack_.pop_back();
  return node;
}

void ExpressionBuilder::push_node(ExprNode* node) { stack_.push_back(node); }

ExpressionBuilder& ExpressionBuilder::binary_op(BinaryOpType op) {
  ExprNode* right = pop();
  ExprNode* left = pop();
  push_node(arena_.make<BinaryOp>(op, left, right));
  return *this;
}

ExpressionBuilder& ExpressionBuilder::unary_op(UnaryOpType op) {
  ExprNode* operand = pop();
  push_node(arena_.make<UnaryOp>(op, operand));
  return *this;
}

// Operand pushers

ExpressionBuilder& ExpressionBuilder::num(double value) {
  push_node(arena_.make<NumberLiteral>(value));
  return *this;
}

ExpressionBuilder& ExpressionBuilder::complex(double real, double imag) {
  push_node(arena_.make<NumberLiteral>(real, imag));
  return *this;
}

ExpressionBuilder& ExpressionBuilder::str(std::string value) {
  push_node(arena_.make<StringLiteral>(std::move(value)));
  return *this;
}

ExpressionBuilder& ExpressionBuilder::id(std::string name) {
  push_node(arena_.make<Identifier>(std::move(name)));
  return *this;
}

ExpressionBuilder& ExpressionBuilder::voltage(std::string node) {
  push_node(arena_.make<CircuitNodeRef>(std::move(node)));
  return *this;
}

ExpressionBuilder& ExpressionBuilder::voltage(std::string node1, std::string node2) {
  push_node(arena_.make<CircuitNodeRef>(std::move(node1), std::move(node2)));
  return *this;
}

ExpressionBuilder& ExpressionBuilder::current(std::string device) {
  push_node(arena_.make<CircuitCurrentRef>(std::move(device)));
  return *this;
}

ExpressionBuilder& ExpressionBuilder::push(ExprNode* node) {
  push_node(node);
  return *this;
}

// Binary operators

ExpressionBuilder& ExpressionBuilder::add() { return binary_op(BinaryOpType::Add); }
ExpressionBuilder& ExpressionBuilder::sub() { return binary_op(BinaryOpType::Subtract); }
ExpressionBuilder& ExpressionBuilder::mul() { return binary_op(BinaryOpType::Multiply); }
ExpressionBuilder& ExpressionBuilder::div() { return binary_op(BinaryOpType::Divide); }
ExpressionBuilder& ExpressionBuilder::pow() { return binary_op(BinaryOpType::Power); }
ExpressionBuilder& ExpressionBuilder::mod() { return binary_op(BinaryOpType::Modulo); }
ExpressionBuilder& ExpressionBuilder::eq() { return binary_op(BinaryOpType::Equal); }
ExpressionBuilder& ExpressionBuilder::ne() { return binary_op(BinaryOpType::NotEqual); }
ExpressionBuilder& ExpressionBuilder::lt() { return binary_op(BinaryOpType::Less); }
ExpressionBuilder& ExpressionBuilder::le() { return binary_op(BinaryOpType::LessEqual); }
ExpressionBuilder& ExpressionBuilder::gt() { return binary_op(BinaryOpType::Greater); }
ExpressionBuilder& ExpressionBuilder::ge() { return binary_op(BinaryOpType::GreaterEqual); }
ExpressionBuilder& ExpressionBuilder::land() { return binary_op(BinaryOpType::LogicalAnd); }
ExpressionBuilder& ExpressionBuilder::lor() { return binary_op(BinaryOpType::LogicalOr); }

// Unary operators

ExpressionBuilder& ExpressionBuilder::neg() { return unary_op(UnaryOpType::Negate); }
ExpressionBuilder& ExpressionBuilder::lnot() { return unary_op(UnaryOpType::LogicalNot); }

// Functions and compound

ExpressionBuilder& ExpressionBuilder::call(std::string name, int n) {
  std::vector<ExprNode*> args(n);
  for (int i = n - 1; i >= 0; --i) {
    args[i] = pop();
  }
  push_node(arena_.make<FunctionCall>(std::move(name), std::move(args)));
  return *this;
}

ExpressionBuilder& ExpressionBuilder::call1(std::string name) {
  return call(std::move(name), 1);
}

ExpressionBuilder& ExpressionBuilder::call2(std::string name) {
  return call(std::move(name), 2);
}

ExpressionBuilder& ExpressionBuilder::index() {
  ExprNode* idx = pop();
  ExprNode* arr = pop();
  push_node(arena_.make<ArrayIndex>(arr, idx));
  return *this;
}

ExpressionBuilder& ExpressionBuilder::ternary() {
  ExprNode* false_expr = pop();
  ExprNode* true_expr = pop();
  ExprNode* condition = pop();
  push_node(arena_.make<TernaryConditional>(condition, true_expr, false_expr));
  return *this;
}

ExpressionBuilder& ExpressionBuilder::array(int n) {
  std::vector<ExprNode*> elements(n);
  for (int i = n - 1; i >= 0; --i) {
    elements[i] = pop();
  }
  push_node(arena_.make<ArrayLiteral>(std::move(elements)));
  return *this;
}

// Result/control

ExprNode* ExpressionBuilder::result() const {
  if (stack_.empty()) {
    throw std::runtime_error("ExpressionBuilder: stack is empty");
  }
  return stack_.back();
}

ExprNode* ExpressionBuilder::build() {
  ExprNode* node = result();
  stack_.pop_back();
  return node;
}

void ExpressionBuilder::clear() { stack_.clear(); }

int ExpressionBuilder::depth() const { return static_cast<int>(stack_.size()); }

}  // namespace spice_expr
