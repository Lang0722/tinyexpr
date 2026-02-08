#include "spice_expr/visitor/tensor_evaluator.h"

#include <set>
#include <string>

#include "spice_expr/array/xtensor.h"
#include "spice_expr/ast/arena.h"
#include "spice_expr/ast/functions.h"
#include "spice_expr/ast/literals.h"
#include "spice_expr/ast/operations.h"
#include "spice_expr/ast/references.h"
#include "spice_expr/function/function_registry.h"
#include "spice_expr/symbol/symbol_table.h"
#include "spice_expr/visitor/evaluator.h"

namespace spice_expr {

// Tensor detection visitor — short-circuits once an array source is found.
namespace {

class TensorDetector : public ConstExprVisitor {
 public:
  explicit TensorDetector(const SymbolTable& symbols) : symbols_(symbols) {}

  bool requires_tensor = false;

  void visit(const NumberLiteral&) override {}
  void visit(const StringLiteral&) override {}
  void visit(const ArrayLiteral&) override { requires_tensor = true; }
  void visit(const Identifier& node) override {
    if (symbols_.is_array(node.name())) {
      requires_tensor = true;
    }
  }
  void visit(const CircuitNodeRef&) override {}
  void visit(const CircuitCurrentRef&) override {}
  void visit(const BinaryOp& node) override {
    if (requires_tensor) return;
    node.left()->accept(*this);
    if (requires_tensor) return;
    node.right()->accept(*this);
  }
  void visit(const UnaryOp& node) override {
    if (requires_tensor) return;
    node.operand()->accept(*this);
  }
  void visit(const FunctionCall& node) override {
    if (requires_tensor) return;
    const std::string& name = node.name();
    if (name == "linspace" || name == "arange" || name == "zeros" || name == "ones") {
      requires_tensor = true;
      return;
    }
    for (const auto* arg : node.arguments()) {
      if (requires_tensor) return;
      arg->accept(*this);
    }
  }
  void visit(const ArrayIndex& node) override {
    if (requires_tensor) return;
    node.array()->accept(*this);
    if (requires_tensor) return;
    node.index()->accept(*this);
  }
  void visit(const TernaryConditional& node) override {
    if (requires_tensor) return;
    node.condition()->accept(*this);
    if (requires_tensor) return;
    node.true_expr()->accept(*this);
    if (requires_tensor) return;
    node.false_expr()->accept(*this);
  }

 private:
  const SymbolTable& symbols_;
};

// Extract a scalar double from an XTensor, throwing EvaluationError if not scalar.
double to_scalar_real(const XTensor& t) {
  if (!t.is_scalar()) {
    throw EvaluationError("Cannot convert array to scalar");
  }
  return t.get_real_at(0);
}

}  // namespace

bool requires_tensor_evaluation(const ExprNode& node, const SymbolTable& symbols) {
  TensorDetector detector(symbols);
  node.accept(detector);
  return detector.requires_tensor;
}

// ---------------------------------------------------------------------------
// TensorEvaluator
// ---------------------------------------------------------------------------

TensorEvaluator::TensorEvaluator(const SymbolTable& symbols, const FunctionRegistry& functions,
                                 const CircuitInterface& circuit, ExprArena* arena)
    : symbols_(symbols), functions_(functions), circuit_(circuit), arena_(arena) {}

XTensor TensorEvaluator::evaluate(const ExprNode& node) {
  while (!stack_.empty()) stack_.pop();
  node.accept(*this);
  if (stack_.empty()) {
    throw EvaluationError("No result produced");
  }
  return pop();
}

XTensor TensorEvaluator::pop() {
  if (stack_.empty()) {
    throw EvaluationError("Stack underflow");
  }
  XTensor val = std::move(stack_.top());
  stack_.pop();
  return val;
}

// ---------------------------------------------------------------------------
// Visitor methods
// ---------------------------------------------------------------------------

void TensorEvaluator::visit(const NumberLiteral& node) {
  if (node.is_complex()) {
    stack_.push(XTensor::scalar(node.complex_value()));
  } else {
    stack_.push(XTensor::scalar(node.real()));
  }
}

void TensorEvaluator::visit(const StringLiteral&) {
  throw EvaluationError("Cannot evaluate string in tensor expression");
}

void TensorEvaluator::visit(const ArrayLiteral& node) {
  std::vector<double> real_values;
  std::vector<std::complex<double>> complex_values;
  bool has_complex = false;

  for (const auto* elem : node.elements()) {
    elem->accept(*this);
    XTensor val = pop();
    if (!val.is_scalar()) {
      throw EvaluationError("Nested arrays not supported in array literals");
    }
    if (val.is_complex()) {
      has_complex = true;
      complex_values.push_back(val.get_complex_at(0));
    } else {
      double d = val.get_real_at(0);
      real_values.push_back(d);
      complex_values.push_back(std::complex<double>(d, 0.0));
    }
  }

  if (has_complex) {
    stack_.push(XTensor(complex_values));
  } else {
    stack_.push(XTensor(real_values));
  }
}

void TensorEvaluator::visit(const Identifier& node) {
  const XTensor* arr = symbols_.lookup_array(node.name());
  if (arr) {
    stack_.push(*arr);
    return;
  }

  auto direct = symbols_.lookup_direct_value(node.name());
  if (direct.has_value()) {
    stack_.push(XTensor::scalar(*direct));
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

void TensorEvaluator::visit(const CircuitNodeRef& node) {
  if (node.is_differential()) {
    stack_.push(
        XTensor::scalar(circuit_.get_differential_voltage_real(node.node1(), node.node2())));
  } else {
    stack_.push(XTensor::scalar(circuit_.get_node_voltage_real(node.node1())));
  }
}

void TensorEvaluator::visit(const CircuitCurrentRef& node) {
  stack_.push(XTensor::scalar(circuit_.get_device_current_real(node.device())));
}

// ---------------------------------------------------------------------------
// Real tensor operations
// ---------------------------------------------------------------------------

XTensor TensorEvaluator::apply_real_binary(BinaryOpType op, const RealXTensor& lhs,
                                           const RealXTensor& rhs) {
  switch (op) {
    case BinaryOpType::Add:
      return XTensor(RealXTensor(lhs + rhs));
    case BinaryOpType::Subtract:
      return XTensor(RealXTensor(lhs - rhs));
    case BinaryOpType::Multiply:
      return XTensor(RealXTensor(lhs * rhs));
    case BinaryOpType::Divide:
      return XTensor(RealXTensor(lhs / rhs));
    case BinaryOpType::Power:
      return XTensor(tt::pow(lhs, rhs));
    case BinaryOpType::Modulo:
      return XTensor(detail::fmod(lhs, rhs));
    case BinaryOpType::Equal:
      return XTensor(detail::cast_to_double(lhs == rhs));
    case BinaryOpType::NotEqual:
      return XTensor(detail::cast_to_double(lhs != rhs));
    case BinaryOpType::Less:
      return XTensor(detail::cast_to_double(lhs < rhs));
    case BinaryOpType::LessEqual:
      return XTensor(detail::cast_to_double(lhs <= rhs));
    case BinaryOpType::Greater:
      return XTensor(detail::cast_to_double(lhs > rhs));
    case BinaryOpType::GreaterEqual:
      return XTensor(detail::cast_to_double(lhs >= rhs));
    default:
      throw EvaluationError("Operator not supported for real tensors");
  }
}

XTensor TensorEvaluator::apply_complex_binary(BinaryOpType op, const ComplexXTensor& lhs,
                                              const ComplexXTensor& rhs) {
  switch (op) {
    case BinaryOpType::Add:
      return XTensor(ComplexXTensor(lhs + rhs));
    case BinaryOpType::Subtract:
      return XTensor(ComplexXTensor(lhs - rhs));
    case BinaryOpType::Multiply:
      return XTensor(ComplexXTensor(lhs * rhs));
    case BinaryOpType::Divide:
      return XTensor(ComplexXTensor(lhs / rhs));
    case BinaryOpType::Power:
      return XTensor(tt::pow(lhs, rhs));
    default:
      throw EvaluationError("Operator not supported for complex tensors");
  }
}

void TensorEvaluator::visit(const BinaryOp& node) {
  node.left()->accept(*this);
  XTensor lhs = pop();
  node.right()->accept(*this);
  XTensor rhs = pop();

  if (lhs.is_complex() || rhs.is_complex()) {
    // Promote to complex — move when already complex, convert when real.
    XTensor lc = lhs.is_complex() ? std::move(lhs) : lhs.to_complex();
    XTensor rc = rhs.is_complex() ? std::move(rhs) : rhs.to_complex();
    stack_.push(apply_complex_binary(node.op_type(), lc.complex(), rc.complex()));
  } else {
    stack_.push(apply_real_binary(node.op_type(), lhs.real(), rhs.real()));
  }
}

// ---------------------------------------------------------------------------
// Real / complex unary operations
// ---------------------------------------------------------------------------

XTensor TensorEvaluator::apply_real_unary(UnaryOpType op, const RealXTensor& operand) {
  switch (op) {
    case UnaryOpType::Negate:
      return XTensor(RealXTensor(-operand));
    case UnaryOpType::Plus:
      return XTensor(RealXTensor(operand));
    case UnaryOpType::LogicalNot:
      return XTensor(detail::cast_to_double(operand == RealXTensor({0.0})));
    default:
      throw EvaluationError("Unknown unary operator");
  }
}

XTensor TensorEvaluator::apply_complex_unary(UnaryOpType op, const ComplexXTensor& operand) {
  switch (op) {
    case UnaryOpType::Negate:
      return XTensor(ComplexXTensor(-operand));
    case UnaryOpType::Plus:
      return XTensor(ComplexXTensor(operand));
    default:
      throw EvaluationError("Operator not supported for complex tensors");
  }
}

void TensorEvaluator::visit(const UnaryOp& node) {
  node.operand()->accept(*this);
  XTensor operand = pop();

  if (operand.is_complex()) {
    stack_.push(apply_complex_unary(node.op_type(), operand.complex()));
  } else {
    stack_.push(apply_real_unary(node.op_type(), operand.real()));
  }
}

// ---------------------------------------------------------------------------
// Real / complex element-wise functions
// ---------------------------------------------------------------------------

XTensor TensorEvaluator::apply_real_func(const std::string& name, const RealXTensor& a) {
  if (name == "sin") return XTensor(tt::sin(a));
  if (name == "cos") return XTensor(tt::cos(a));
  if (name == "tan") return XTensor(tt::tan(a));
  if (name == "asin") return XTensor(tt::asin(a));
  if (name == "acos") return XTensor(tt::acos(a));
  if (name == "atan") return XTensor(tt::atan(a));
  if (name == "sinh") return XTensor(tt::sinh(a));
  if (name == "cosh") return XTensor(tt::cosh(a));
  if (name == "tanh") return XTensor(tt::tanh(a));
  if (name == "exp") return XTensor(tt::exp(a));
  if (name == "log") return XTensor(tt::log(a));
  if (name == "log10") return XTensor(tt::log10(a));
  if (name == "sqrt") return XTensor(tt::sqrt(a));
  if (name == "abs") return XTensor(tt::abs(a));
  if (name == "floor") return XTensor(tt::floor(a));
  if (name == "ceil") return XTensor(tt::ceil(a));
  if (name == "round") return XTensor(tt::round(a));
  throw EvaluationError("Unknown function: " + name);
}

XTensor TensorEvaluator::apply_complex_func(const std::string& name, const ComplexXTensor& a) {
  if (name == "abs") return XTensor(tt::abs(a));
  if (name == "sin") return XTensor(tt::sin(a));
  if (name == "cos") return XTensor(tt::cos(a));
  if (name == "tan") return XTensor(tt::tan(a));
  if (name == "exp") return XTensor(tt::exp(a));
  if (name == "log") return XTensor(tt::log(a));
  if (name == "sqrt") return XTensor(tt::sqrt(a));
  throw EvaluationError("Function " + name + " not supported for complex tensors");
}

// ---------------------------------------------------------------------------
// FunctionCall dispatch
// ---------------------------------------------------------------------------

void TensorEvaluator::visit(const FunctionCall& node) {
  const std::string& name = node.name();

  // Array-producing functions — arguments are scalar.
  if (name == "linspace") {
    if (node.argument_count() != 3) {
      throw EvaluationError("linspace requires 3 arguments");
    }
    node.arguments()[0]->accept(*this);
    double start = to_scalar_real(pop());
    node.arguments()[1]->accept(*this);
    double stop = to_scalar_real(pop());
    node.arguments()[2]->accept(*this);
    auto num = static_cast<size_t>(to_scalar_real(pop()));
    stack_.push(linspace(start, stop, num));
    return;
  }

  if (name == "arange") {
    if (node.argument_count() < 2 || node.argument_count() > 3) {
      throw EvaluationError("arange requires 2 or 3 arguments");
    }
    node.arguments()[0]->accept(*this);
    double start = to_scalar_real(pop());
    node.arguments()[1]->accept(*this);
    double stop = to_scalar_real(pop());
    double step = 1.0;
    if (node.argument_count() == 3) {
      node.arguments()[2]->accept(*this);
      step = to_scalar_real(pop());
    }
    stack_.push(arange(start, stop, step));
    return;
  }

  if (name == "zeros") {
    if (node.argument_count() != 1) {
      throw EvaluationError("zeros requires 1 argument");
    }
    node.arguments()[0]->accept(*this);
    auto n = static_cast<size_t>(to_scalar_real(pop()));
    stack_.push(zeros(n));
    return;
  }

  if (name == "ones") {
    if (node.argument_count() != 1) {
      throw EvaluationError("ones requires 1 argument");
    }
    node.arguments()[0]->accept(*this);
    auto n = static_cast<size_t>(to_scalar_real(pop()));
    stack_.push(ones(n));
    return;
  }

  // Reduction functions — return scalar tensors.
  if (name == "sum" || name == "mean" || name == "min" || name == "max" || name == "len") {
    if (node.argument_count() != 1) {
      throw EvaluationError(name + " requires 1 argument");
    }
    node.arguments()[0]->accept(*this);
    XTensor arg = pop();

    if (name == "sum") {
      stack_.push(XTensor::scalar(arg.sum_real()));
    } else if (name == "mean") {
      stack_.push(XTensor::scalar(arg.mean_real()));
    } else if (name == "min") {
      stack_.push(XTensor::scalar(arg.min_real()));
    } else if (name == "max") {
      stack_.push(XTensor::scalar(arg.max_real()));
    } else {
      stack_.push(XTensor::scalar(static_cast<double>(arg.size())));
    }
    return;
  }

  // Element-wise math functions — dispatch to separated real/complex paths.
  static const std::set<std::string> elementwise_funcs = {
      "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh",
      "exp", "log", "log10", "sqrt", "abs", "floor", "ceil", "round"};

  if (elementwise_funcs.count(name) && node.argument_count() == 1) {
    node.arguments()[0]->accept(*this);
    XTensor arg = pop();

    if (arg.is_complex()) {
      stack_.push(apply_complex_func(name, arg.complex()));
    } else {
      stack_.push(apply_real_func(name, arg.real()));
    }
    return;
  }

  // real/imag extraction.
  if ((name == "real" || name == "imag") && node.argument_count() == 1) {
    node.arguments()[0]->accept(*this);
    XTensor arg = pop();

    if (arg.is_complex()) {
      if (name == "real") {
        stack_.push(XTensor(detail::extract_real(arg.complex())));
      } else {
        stack_.push(XTensor(detail::extract_imag(arg.complex())));
      }
    } else {
      if (name == "real") {
        stack_.push(std::move(arg));
      } else {
        auto s = arg.shape();
        stack_.push(XTensor(tt::zeros<double>(tt::shape_t(s.begin(), s.end()))));
      }
    }
    return;
  }

  // Fall back to scalar evaluation for other functions.
  std::vector<double> args;
  for (const auto* a : node.arguments()) {
    a->accept(*this);
    XTensor val = pop();
    if (!val.is_scalar()) {
      throw EvaluationError("Function " + name + " does not support array arguments");
    }
    args.push_back(val.get_real_at(0));
  }
  stack_.push(XTensor::scalar(functions_.evaluate_real_builtin(name, args)));
}

// ---------------------------------------------------------------------------
// ArrayIndex / TernaryConditional
// ---------------------------------------------------------------------------

void TensorEvaluator::visit(const ArrayIndex& node) {
  node.index()->accept(*this);
  XTensor idx_tensor = pop();
  if (!idx_tensor.is_scalar()) {
    throw EvaluationError("Array indexing with array indices not supported yet");
  }
  int idx = static_cast<int>(idx_tensor.get_real_at(0));

  node.array()->accept(*this);
  XTensor arr = pop();

  if (arr.is_scalar()) {
    throw EvaluationError("Cannot index a scalar value");
  }

  if (idx < 0 || static_cast<size_t>(idx) >= arr.size()) {
    throw EvaluationError("Array index out of bounds");
  }

  if (arr.is_real()) {
    stack_.push(XTensor::scalar(arr.get_real_at(idx)));
  } else {
    stack_.push(XTensor::scalar(arr.get_complex_at(idx)));
  }
}

void TensorEvaluator::visit(const TernaryConditional& node) {
  node.condition()->accept(*this);
  XTensor cond = pop();

  if (!cond.is_scalar()) {
    throw EvaluationError("Array conditions in ternary not supported yet");
  }

  if (cond.get_real_at(0) != 0.0) {
    node.true_expr()->accept(*this);
  } else {
    node.false_expr()->accept(*this);
  }
}

}  // namespace spice_expr
