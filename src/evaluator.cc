#include "spice_expr/visitor/evaluator.h"

#include <cmath>

#include "spice_expr/ast/arena.h"
#include "spice_expr/ast/functions.h"
#include "spice_expr/ast/literals.h"
#include "spice_expr/ast/operations.h"
#include "spice_expr/ast/references.h"
#include "spice_expr/function/function_registry.h"
#include "spice_expr/symbol/symbol_table.h"

namespace spice_expr {

namespace {

class ComplexDetector : public ConstExprVisitor {
 public:
  bool requiresComplex = false;

  void visit(const NumberLiteral& node) override {
    if (node.is_complex())
      requiresComplex = true;
  }
  void visit(const StringLiteral&) override {}
  void visit(const ArrayLiteral& node) override {
    for (auto* elem : node.elements()) {
      elem->accept(*this);
    }
  }
  void visit(const Identifier&) override {}
  void visit(const CircuitNodeRef&) override {}
  void visit(const CircuitCurrentRef&) override {}
  void visit(const BinaryOp& node) override {
    node.left()->accept(*this);
    node.right()->accept(*this);
  }
  void visit(const UnaryOp& node) override { node.operand()->accept(*this); }
  void visit(const FunctionCall& node) override {
    for (auto* arg : node.arguments()) {
      arg->accept(*this);
    }
  }
  void visit(const ArrayIndex& node) override {
    node.array()->accept(*this);
    node.index()->accept(*this);
  }
  void visit(const TernaryConditional& node) override {
    node.condition()->accept(*this);
    node.true_expr()->accept(*this);
    node.false_expr()->accept(*this);
  }
};

}  // namespace

bool requires_complex_evaluation(const ExprNode& node) {
  ComplexDetector detector;
  node.accept(detector);
  return detector.requiresComplex;
}

// RealEvaluator

RealEvaluator::RealEvaluator(const SymbolTable& symbols, const FunctionRegistry& functions,
                             const CircuitInterface& circuit, ExprArena* arena)
    : symbols_(symbols), functions_(functions), circuit_(circuit), arena_(arena) {}

double RealEvaluator::evaluate(const ExprNode& node) {
  while (!stack_.empty())
    stack_.pop();
  node.accept(*this);
  return result();
}

double RealEvaluator::result() const {
  if (stack_.empty()) {
    throw EvaluationError("No result on stack");
  }
  return const_cast<RealEvaluator*>(this)->stack_.top();
}

double RealEvaluator::pop() {
  if (stack_.empty()) {
    throw EvaluationError("Stack underflow");
  }
  double val = stack_.top();
  stack_.pop();
  return val;
}

void RealEvaluator::visit(const NumberLiteral& node) {
  stack_.push(node.real());
}

void RealEvaluator::visit(const StringLiteral&) {
  throw EvaluationError("Cannot evaluate string as number");
}

void RealEvaluator::visit(const ArrayLiteral&) {
  throw EvaluationError("Cannot evaluate array as scalar");
}

void RealEvaluator::visit(const Identifier& node) {
  // Check for directly bound values (used by user function parameters)
  auto direct = symbols_.lookup_direct_value(node.name());
  if (direct.has_value()) {
    stack_.push(*direct);
    return;
  }

  const SymbolEntry* entry = symbols_.lookup(node.name());
  if (!entry) {
    throw EvaluationError("Undefined parameter: " + node.name());
  }
  if (!entry->expression) {
    throw EvaluationError("Parameter has no expression: " + node.name());
  }
  entry->expression->accept(*this);
}

void RealEvaluator::visit(const CircuitNodeRef& node) {
  if (node.is_differential()) {
    stack_.push(circuit_.get_differential_voltage_real(node.node1(), node.node2()));
  } else {
    stack_.push(circuit_.get_node_voltage_real(node.node1()));
  }
}

void RealEvaluator::visit(const CircuitCurrentRef& node) {
  stack_.push(circuit_.get_device_current_real(node.device()));
}

void RealEvaluator::visit(const BinaryOp& node) {
  node.left()->accept(*this);
  node.right()->accept(*this);
  double right = pop();
  double left = pop();

  double result;
  switch (node.op_type()) {
    case BinaryOpType::Add:
      result = left + right;
      break;
    case BinaryOpType::Subtract:
      result = left - right;
      break;
    case BinaryOpType::Multiply:
      result = left * right;
      break;
    case BinaryOpType::Divide:
      if (right == 0.0)
        throw EvaluationError("Division by zero");
      result = left / right;
      break;
    case BinaryOpType::Power:
      result = std::pow(left, right);
      break;
    case BinaryOpType::Modulo:
      if (right == 0.0)
        throw EvaluationError("Modulo by zero");
      result = std::fmod(left, right);
      break;
    case BinaryOpType::Equal:
      result = (left == right) ? 1.0 : 0.0;
      break;
    case BinaryOpType::NotEqual:
      result = (left != right) ? 1.0 : 0.0;
      break;
    case BinaryOpType::Less:
      result = (left < right) ? 1.0 : 0.0;
      break;
    case BinaryOpType::LessEqual:
      result = (left <= right) ? 1.0 : 0.0;
      break;
    case BinaryOpType::Greater:
      result = (left > right) ? 1.0 : 0.0;
      break;
    case BinaryOpType::GreaterEqual:
      result = (left >= right) ? 1.0 : 0.0;
      break;
    case BinaryOpType::LogicalAnd:
      result = (left != 0.0 && right != 0.0) ? 1.0 : 0.0;
      break;
    case BinaryOpType::LogicalOr:
      result = (left != 0.0 || right != 0.0) ? 1.0 : 0.0;
      break;
    default:
      throw EvaluationError("Unknown binary operator");
  }
  stack_.push(result);
}

void RealEvaluator::visit(const UnaryOp& node) {
  node.operand()->accept(*this);
  double operand = pop();

  double result;
  switch (node.op_type()) {
    case UnaryOpType::Negate:
      result = -operand;
      break;
    case UnaryOpType::Plus:
      result = operand;
      break;
    case UnaryOpType::LogicalNot:
      result = (operand == 0.0) ? 1.0 : 0.0;
      break;
    default:
      throw EvaluationError("Unknown unary operator");
  }
  stack_.push(result);
}

void RealEvaluator::visit(const FunctionCall& node) {
  const UserFunction* userFunc = functions_.lookup_user(node.name());
  if (userFunc) {
    if (node.argument_count() != userFunc->parameter_count()) {
      throw EvaluationError("Wrong number of arguments for function " + node.name());
    }
    std::vector<double> argValues;
    for (auto* arg : node.arguments()) {
      arg->accept(*this);
      argValues.push_back(pop());
    }
    // Use bind_value to avoid arena allocation for function parameters
    SymbolTable localScope(const_cast<SymbolTable*>(&symbols_));
    for (size_t i = 0; i < userFunc->parameters().size(); ++i) {
      localScope.bind_value(userFunc->parameters()[i], argValues[i]);
    }
    RealEvaluator localEval(localScope, functions_, circuit_, arena_);
    stack_.push(localEval.evaluate(*userFunc->body()));
    return;
  }

  std::vector<double> args;
  for (auto* arg : node.arguments()) {
    arg->accept(*this);
    args.push_back(pop());
  }
  stack_.push(functions_.evaluate_real_builtin(node.name(), args));
}

void RealEvaluator::visit(const ArrayIndex& node) {
  node.index()->accept(*this);
  int idx = static_cast<int>(pop());

  if (node.array()->type() != NodeType::ArrayLiteral) {
    throw EvaluationError("Array indexing requires array literal");
  }
  auto& arr = static_cast<const ArrayLiteral&>(*node.array());
  if (idx < 0 || static_cast<size_t>(idx) >= arr.element_count()) {
    throw EvaluationError("Array index out of bounds");
  }
  arr.elements()[idx]->accept(*this);
}

void RealEvaluator::visit(const TernaryConditional& node) {
  node.condition()->accept(*this);
  double cond = pop();
  if (cond != 0.0) {
    node.true_expr()->accept(*this);
  } else {
    node.false_expr()->accept(*this);
  }
}

// ComplexEvaluator

ComplexEvaluator::ComplexEvaluator(const SymbolTable& symbols, const FunctionRegistry& functions,
                                   const CircuitInterface& circuit, ExprArena* arena)
    : symbols_(symbols), functions_(functions), circuit_(circuit), arena_(arena) {}

void ComplexEvaluator::warn_complex_fallback(const char* op_name) {
  std::string op{op_name};
  if (warned_ops_.find(op) == warned_ops_.end()) {
    warned_ops_.insert(op);
    std::cerr << "Warning: operator '" << op_name
              << "' not supported for complex numbers, using real part\n";
  }
}

std::complex<double> ComplexEvaluator::evaluate(const ExprNode& node) {
  while (!stack_.empty())
    stack_.pop();
  node.accept(*this);
  return result();
}

std::complex<double> ComplexEvaluator::result() const {
  if (stack_.empty()) {
    throw EvaluationError("No result on stack");
  }
  return const_cast<ComplexEvaluator*>(this)->stack_.top();
}

std::complex<double> ComplexEvaluator::pop() {
  if (stack_.empty()) {
    throw EvaluationError("Stack underflow");
  }
  auto val = stack_.top();
  stack_.pop();
  return val;
}

void ComplexEvaluator::visit(const NumberLiteral& node) {
  stack_.push(node.complex_value());
}

void ComplexEvaluator::visit(const StringLiteral&) {
  throw EvaluationError("Cannot evaluate string as number");
}

void ComplexEvaluator::visit(const ArrayLiteral&) {
  throw EvaluationError("Cannot evaluate array as scalar");
}

void ComplexEvaluator::visit(const Identifier& node) {
  // Check for directly bound values (used by user function parameters)
  auto direct = symbols_.lookup_direct_value(node.name());
  if (direct.has_value()) {
    stack_.push(std::complex<double>(*direct, 0.0));
    return;
  }

  const SymbolEntry* entry = symbols_.lookup(node.name());
  if (!entry) {
    throw EvaluationError("Undefined parameter: " + node.name());
  }
  if (!entry->expression) {
    throw EvaluationError("Parameter has no expression: " + node.name());
  }
  entry->expression->accept(*this);
}

void ComplexEvaluator::visit(const CircuitNodeRef& node) {
  if (node.is_differential()) {
    stack_.push(circuit_.get_differential_voltage_complex(node.node1(), node.node2()));
  } else {
    stack_.push(circuit_.get_node_voltage_complex(node.node1()));
  }
}

void ComplexEvaluator::visit(const CircuitCurrentRef& node) {
  stack_.push(circuit_.get_device_current_complex(node.device()));
}

void ComplexEvaluator::visit(const BinaryOp& node) {
  node.left()->accept(*this);
  node.right()->accept(*this);
  auto right = pop();
  auto left = pop();

  std::complex<double> result;
  switch (node.op_type()) {
    case BinaryOpType::Add:
      result = left + right;
      break;
    case BinaryOpType::Subtract:
      result = left - right;
      break;
    case BinaryOpType::Multiply:
      result = left * right;
      break;
    case BinaryOpType::Divide:
      if (right == std::complex<double>(0.0, 0.0))
        throw EvaluationError("Division by zero");
      result = left / right;
      break;
    case BinaryOpType::Power:
      result = std::pow(left, right);
      break;
    case BinaryOpType::Equal:
      result = (left == right) ? std::complex<double>(1.0, 0.0) : std::complex<double>(0.0, 0.0);
      break;
    case BinaryOpType::NotEqual:
      result = (left != right) ? std::complex<double>(1.0, 0.0) : std::complex<double>(0.0, 0.0);
      break;
    // Operators that fall back to real-part evaluation with one-time warning
    case BinaryOpType::Modulo: {
      warn_complex_fallback(binaryOpTypeToString(node.op_type()));
      double l = left.real(), r = right.real();
      if (r == 0.0)
        throw EvaluationError("Modulo by zero");
      result = std::complex<double>(std::fmod(l, r), 0.0);
      break;
    }
    case BinaryOpType::Less: {
      warn_complex_fallback(binaryOpTypeToString(node.op_type()));
      result = (left.real() < right.real()) ? std::complex<double>(1.0, 0.0)
                                            : std::complex<double>(0.0, 0.0);
      break;
    }
    case BinaryOpType::LessEqual: {
      warn_complex_fallback(binaryOpTypeToString(node.op_type()));
      result = (left.real() <= right.real()) ? std::complex<double>(1.0, 0.0)
                                             : std::complex<double>(0.0, 0.0);
      break;
    }
    case BinaryOpType::Greater: {
      warn_complex_fallback(binaryOpTypeToString(node.op_type()));
      result = (left.real() > right.real()) ? std::complex<double>(1.0, 0.0)
                                            : std::complex<double>(0.0, 0.0);
      break;
    }
    case BinaryOpType::GreaterEqual: {
      warn_complex_fallback(binaryOpTypeToString(node.op_type()));
      result = (left.real() >= right.real()) ? std::complex<double>(1.0, 0.0)
                                             : std::complex<double>(0.0, 0.0);
      break;
    }
    case BinaryOpType::LogicalAnd: {
      warn_complex_fallback(binaryOpTypeToString(node.op_type()));
      result = (left.real() != 0.0 && right.real() != 0.0) ? std::complex<double>(1.0, 0.0)
                                                           : std::complex<double>(0.0, 0.0);
      break;
    }
    case BinaryOpType::LogicalOr: {
      warn_complex_fallback(binaryOpTypeToString(node.op_type()));
      result = (left.real() != 0.0 || right.real() != 0.0) ? std::complex<double>(1.0, 0.0)
                                                           : std::complex<double>(0.0, 0.0);
      break;
    }
    default:
      throw EvaluationError("Unknown binary operator");
  }
  stack_.push(result);
}

void ComplexEvaluator::visit(const UnaryOp& node) {
  node.operand()->accept(*this);
  auto operand = pop();

  std::complex<double> result;
  switch (node.op_type()) {
    case UnaryOpType::Negate:
      result = -operand;
      break;
    case UnaryOpType::Plus:
      result = operand;
      break;
    case UnaryOpType::LogicalNot: {
      warn_complex_fallback(unaryOpTypeToString(node.op_type()));
      result = (operand.real() == 0.0) ? std::complex<double>(1.0, 0.0)
                                       : std::complex<double>(0.0, 0.0);
      break;
    }
    default:
      throw EvaluationError("Unknown unary operator");
  }
  stack_.push(result);
}

void ComplexEvaluator::visit(const FunctionCall& node) {
  const UserFunction* userFunc = functions_.lookup_user(node.name());
  if (userFunc) {
    if (node.argument_count() != userFunc->parameter_count()) {
      throw EvaluationError("Wrong number of arguments for function " + node.name());
    }
    std::vector<std::complex<double>> argValues;
    for (auto* arg : node.arguments()) {
      arg->accept(*this);
      argValues.push_back(pop());
    }
    // Create NumberLiteral nodes for complex parameters using the arena
    SymbolTable localScope(const_cast<SymbolTable*>(&symbols_));
    for (size_t i = 0; i < userFunc->parameters().size(); ++i) {
      auto* paramExpr =
          arena_->make<NumberLiteral>(argValues[i].real(), argValues[i].imag());
      localScope.define(userFunc->parameters()[i], paramExpr);
    }
    ComplexEvaluator localEval(localScope, functions_, circuit_, arena_);
    stack_.push(localEval.evaluate(*userFunc->body()));
    return;
  }

  std::vector<std::complex<double>> args;
  for (auto* arg : node.arguments()) {
    arg->accept(*this);
    args.push_back(pop());
  }
  stack_.push(functions_.evaluate_complex_builtin(node.name(), args));
}

void ComplexEvaluator::visit(const ArrayIndex& node) {
  node.index()->accept(*this);
  int idx = static_cast<int>(pop().real());

  if (node.array()->type() != NodeType::ArrayLiteral) {
    throw EvaluationError("Array indexing requires array literal");
  }
  auto& arr = static_cast<const ArrayLiteral&>(*node.array());
  if (idx < 0 || static_cast<size_t>(idx) >= arr.element_count()) {
    throw EvaluationError("Array index out of bounds");
  }
  arr.elements()[idx]->accept(*this);
}

void ComplexEvaluator::visit(const TernaryConditional& node) {
  node.condition()->accept(*this);
  auto cond = pop();
  if (cond != std::complex<double>(0.0, 0.0)) {
    node.true_expr()->accept(*this);
  } else {
    node.false_expr()->accept(*this);
  }
}

}  // namespace spice_expr
