#include "spice_expr/visitor/array_evaluator.h"

#include "spice_expr/array/xtensor.h"
#include "spice_expr/ast/arena.h"
#include "spice_expr/ast/functions.h"
#include "spice_expr/ast/literals.h"
#include "spice_expr/ast/operations.h"
#include "spice_expr/ast/references.h"
#include "spice_expr/function/function_registry.h"
#include "spice_expr/symbol/symbol_table.h"

namespace spice_expr {

// Helper functions to create XTensor from tensor arrays
namespace {

XTensor make_xtensor(RealXTensor arr) {
  return XTensor(std::move(arr));
}

XTensor make_xtensor(ComplexXTensor arr) {
  return XTensor(std::move(arr));
}

}  // namespace

// EvalValueOps implementation
// With unified tensor approach, all values are XTensor (scalars are 1-element tensors)

bool EvalValueOps::is_scalar(const EvalValue& v) {
  if (std::holds_alternative<double>(v) || std::holds_alternative<std::complex<double>>(v)) {
    return true;
  }
  if (std::holds_alternative<XTensor>(v)) {
    return std::get<XTensor>(v).is_scalar();
  }
  return false;
}

bool EvalValueOps::is_array(const EvalValue& v) {
  if (std::holds_alternative<XTensor>(v)) {
    return !std::get<XTensor>(v).is_scalar();
  }
  return false;
}

bool EvalValueOps::is_real(const EvalValue& v) {
  if (std::holds_alternative<double>(v)) {
    return true;
  }
  if (std::holds_alternative<XTensor>(v)) {
    return std::get<XTensor>(v).is_real();
  }
  return false;
}

bool EvalValueOps::is_complex(const EvalValue& v) {
  if (std::holds_alternative<std::complex<double>>(v)) {
    return true;
  }
  if (std::holds_alternative<XTensor>(v)) {
    return std::get<XTensor>(v).is_complex();
  }
  return false;
}

double EvalValueOps::to_real(const EvalValue& v) {
  if (std::holds_alternative<double>(v)) {
    return std::get<double>(v);
  }
  if (std::holds_alternative<std::complex<double>>(v)) {
    return std::get<std::complex<double>>(v).real();
  }
  if (std::holds_alternative<XTensor>(v)) {
    const XTensor& t = std::get<XTensor>(v);
    if (t.is_scalar()) {
      return t.get_real_at(0);
    }
  }
  throw EvaluationError("Cannot convert array to scalar");
}

std::complex<double> EvalValueOps::to_complex(const EvalValue& v) {
  if (std::holds_alternative<double>(v)) {
    return std::complex<double>(std::get<double>(v), 0.0);
  }
  if (std::holds_alternative<std::complex<double>>(v)) {
    return std::get<std::complex<double>>(v);
  }
  if (std::holds_alternative<XTensor>(v)) {
    const XTensor& t = std::get<XTensor>(v);
    if (t.is_scalar()) {
      return t.get_complex_at(0);
    }
  }
  throw EvaluationError("Cannot convert array to scalar");
}

XTensor EvalValueOps::to_array(const EvalValue& v) {
  if (std::holds_alternative<XTensor>(v)) {
    return std::get<XTensor>(v);
  }
  if (std::holds_alternative<double>(v)) {
    return XTensor::scalar(std::get<double>(v));
  }
  if (std::holds_alternative<std::complex<double>>(v)) {
    return XTensor::scalar(std::get<std::complex<double>>(v));
  }
  throw EvaluationError("Unknown value type");
}

// Array detection visitor
namespace {

class ArrayDetector : public ConstExprVisitor {
 public:
  explicit ArrayDetector(const SymbolTable& symbols) : symbols_(symbols) {}

  bool requires_array = false;

  void visit(const NumberLiteral&) override {}
  void visit(const StringLiteral&) override {}
  void visit(const ArrayLiteral&) override { requires_array = true; }
  void visit(const Identifier& node) override {
    if (symbols_.is_array(node.name())) {
      requires_array = true;
    }
  }
  void visit(const CircuitNodeRef&) override {}
  void visit(const CircuitCurrentRef&) override {}
  void visit(const BinaryOp& node) override {
    node.left()->accept(*this);
    node.right()->accept(*this);
  }
  void visit(const UnaryOp& node) override { node.operand()->accept(*this); }
  void visit(const FunctionCall& node) override {
    // Check for array-producing functions
    const std::string& name = node.name();
    if (name == "linspace" || name == "arange" || name == "zeros" || name == "ones") {
      requires_array = true;
    }
    for (const auto* arg : node.arguments()) {
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

 private:
  const SymbolTable& symbols_;
};

}  // namespace

bool requires_array_evaluation(const ExprNode& node, const SymbolTable& symbols) {
  ArrayDetector detector(symbols);
  node.accept(detector);
  return detector.requires_array;
}

// ArrayEvaluator implementation

ArrayEvaluator::ArrayEvaluator(const SymbolTable& symbols, const FunctionRegistry& functions,
                               const CircuitInterface& circuit, ExprArena* arena)
    : symbols_(symbols), functions_(functions), circuit_(circuit), arena_(arena) {}

EvalValue ArrayEvaluator::evaluate(const ExprNode& node) {
  while (!stack_.empty())
    stack_.pop();
  node.accept(*this);
  return result();
}

EvalValue ArrayEvaluator::pop() {
  if (stack_.empty()) {
    throw EvaluationError("Stack underflow");
  }
  EvalValue val = std::move(stack_.top());
  stack_.pop();
  return val;
}

void ArrayEvaluator::visit(const NumberLiteral& node) {
  if (node.is_complex()) {
    stack_.push(XTensor::scalar(node.complex_value()));
  } else {
    stack_.push(XTensor::scalar(node.real()));
  }
}

void ArrayEvaluator::visit(const StringLiteral&) {
  throw EvaluationError("Cannot evaluate string in array expression");
}

void ArrayEvaluator::visit(const ArrayLiteral& node) {
  // Evaluate each element and build an XTensor
  std::vector<double> real_values;
  std::vector<std::complex<double>> complex_values;
  bool has_complex = false;

  for (const auto* elem : node.elements()) {
    elem->accept(*this);
    EvalValue val = pop();
    if (!EvalValueOps::is_scalar(val)) {
      throw EvaluationError("Nested arrays not supported in array literals");
    }
    if (EvalValueOps::is_complex(val)) {
      has_complex = true;
      complex_values.push_back(EvalValueOps::to_complex(val));
    } else {
      double d = EvalValueOps::to_real(val);
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

void ArrayEvaluator::visit(const Identifier& node) {
  // Check for array parameter first
  const XTensor* arr = symbols_.lookup_array(node.name());
  if (arr) {
    stack_.push(*arr);
    return;
  }

  // Check for directly bound scalar values - convert to scalar tensor
  auto direct = symbols_.lookup_direct_value(node.name());
  if (direct.has_value()) {
    stack_.push(XTensor::scalar(*direct));
    return;
  }

  // Look up symbol expression
  const SymbolEntry* entry = symbols_.lookup(node.name());
  if (!entry) {
    throw EvaluationError("Undefined parameter: " + node.name());
  }
  if (!entry->expression) {
    throw EvaluationError("Parameter has no expression: " + node.name());
  }
  entry->expression->accept(*this);
}

void ArrayEvaluator::visit(const CircuitNodeRef& node) {
  if (node.is_differential()) {
    stack_.push(XTensor::scalar(circuit_.get_differential_voltage_real(node.node1(), node.node2())));
  } else {
    stack_.push(XTensor::scalar(circuit_.get_node_voltage_real(node.node1())));
  }
}

void ArrayEvaluator::visit(const CircuitCurrentRef& node) {
  stack_.push(XTensor::scalar(circuit_.get_device_current_real(node.device())));
}

void ArrayEvaluator::visit(const BinaryOp& node) {
  node.left()->accept(*this);
  EvalValue lhs = pop();
  node.right()->accept(*this);
  EvalValue rhs = pop();
  stack_.push(apply_binary_op(node.op_type(), lhs, rhs));
}

EvalValue ArrayEvaluator::apply_binary_op(BinaryOpType op, const EvalValue& lhs,
                                          const EvalValue& rhs) {
  // Unified tensor approach: all values are XTensor (shape {1} for scalars)
  // Broadcasting handles scalar-scalar, scalar-array, and array-array operations uniformly
  const XTensor& la = EvalValueOps::to_array(lhs);
  const XTensor& ra = EvalValueOps::to_array(rhs);

  // Check if we need complex arithmetic
  bool need_complex = la.is_complex() || ra.is_complex();

  if (need_complex) {
    ComplexXTensor larr;
    if (la.is_complex()) {
      larr = la.complex();
    } else {
      larr = detail::cast_to_complex(la.real());
    }
    ComplexXTensor rarr;
    if (ra.is_complex()) {
      rarr = ra.complex();
    } else {
      rarr = detail::cast_to_complex(ra.real());
    }

    switch (op) {
      case BinaryOpType::Add:
        return make_xtensor(larr + rarr);
      case BinaryOpType::Subtract:
        return make_xtensor(larr - rarr);
      case BinaryOpType::Multiply:
        return make_xtensor(larr * rarr);
      case BinaryOpType::Divide:
        return make_xtensor(larr / rarr);
      case BinaryOpType::Power:
        return make_xtensor(tt::pow(larr, rarr));
      default:
        throw EvaluationError("Operator not supported for complex arrays");
    }
  } else {
    const RealXTensor& larr = la.real();
    const RealXTensor& rarr = ra.real();

    switch (op) {
      case BinaryOpType::Add:
        return make_xtensor(larr + rarr);
      case BinaryOpType::Subtract:
        return make_xtensor(larr - rarr);
      case BinaryOpType::Multiply:
        return make_xtensor(larr * rarr);
      case BinaryOpType::Divide:
        return make_xtensor(larr / rarr);
      case BinaryOpType::Power:
        return make_xtensor(tt::pow(larr, rarr));
      case BinaryOpType::Modulo:
        return make_xtensor(detail::fmod(larr, rarr));
      case BinaryOpType::Equal:
        return make_xtensor(detail::cast_to_double(larr == rarr));
      case BinaryOpType::NotEqual:
        return make_xtensor(detail::cast_to_double(larr != rarr));
      case BinaryOpType::Less:
        return make_xtensor(detail::cast_to_double(larr < rarr));
      case BinaryOpType::LessEqual:
        return make_xtensor(detail::cast_to_double(larr <= rarr));
      case BinaryOpType::Greater:
        return make_xtensor(detail::cast_to_double(larr > rarr));
      case BinaryOpType::GreaterEqual:
        return make_xtensor(detail::cast_to_double(larr >= rarr));
      default:
        throw EvaluationError("Operator not supported for arrays");
    }
  }
}

void ArrayEvaluator::visit(const UnaryOp& node) {
  node.operand()->accept(*this);
  EvalValue operand = pop();
  stack_.push(apply_unary_op(node.op_type(), operand));
}

EvalValue ArrayEvaluator::apply_unary_op(UnaryOpType op, const EvalValue& operand) {
  const XTensor& arr = EvalValueOps::to_array(operand);
  if (arr.is_complex()) {
    const ComplexXTensor& a = arr.complex();
    switch (op) {
      case UnaryOpType::Negate:
        return make_xtensor(ComplexXTensor(-a));
      case UnaryOpType::Plus:
        return make_xtensor(ComplexXTensor(a));
      default:
        throw EvaluationError("Operator not supported for complex arrays");
    }
  } else {
    const RealXTensor& a = arr.real();
    switch (op) {
      case UnaryOpType::Negate:
        return make_xtensor(RealXTensor(-a));
      case UnaryOpType::Plus:
        return make_xtensor(RealXTensor(a));
      case UnaryOpType::LogicalNot:
        return make_xtensor(detail::cast_to_double(a == RealXTensor({0.0})));
      default:
        throw EvaluationError("Unknown unary operator");
    }
  }
}

void ArrayEvaluator::visit(const FunctionCall& node) {
  const std::string& name = node.name();

  // Handle array-producing functions
  if (name == "linspace") {
    if (node.argument_count() != 3) {
      throw EvaluationError("linspace requires 3 arguments");
    }
    node.arguments()[0]->accept(*this);
    double start = EvalValueOps::to_real(pop());
    node.arguments()[1]->accept(*this);
    double stop = EvalValueOps::to_real(pop());
    node.arguments()[2]->accept(*this);
    size_t num = static_cast<size_t>(EvalValueOps::to_real(pop()));
    stack_.push(linspace(start, stop, num));
    return;
  }

  if (name == "arange") {
    if (node.argument_count() < 2 || node.argument_count() > 3) {
      throw EvaluationError("arange requires 2 or 3 arguments");
    }
    node.arguments()[0]->accept(*this);
    double start = EvalValueOps::to_real(pop());
    node.arguments()[1]->accept(*this);
    double stop = EvalValueOps::to_real(pop());
    double step = 1.0;
    if (node.argument_count() == 3) {
      node.arguments()[2]->accept(*this);
      step = EvalValueOps::to_real(pop());
    }
    stack_.push(arange(start, stop, step));
    return;
  }

  if (name == "zeros") {
    if (node.argument_count() != 1) {
      throw EvaluationError("zeros requires 1 argument");
    }
    node.arguments()[0]->accept(*this);
    size_t n = static_cast<size_t>(EvalValueOps::to_real(pop()));
    stack_.push(zeros(n));
    return;
  }

  if (name == "ones") {
    if (node.argument_count() != 1) {
      throw EvaluationError("ones requires 1 argument");
    }
    node.arguments()[0]->accept(*this);
    size_t n = static_cast<size_t>(EvalValueOps::to_real(pop()));
    stack_.push(ones(n));
    return;
  }

  // Handle reduction functions
  if (name == "sum" || name == "mean" || name == "min" || name == "max" || name == "len") {
    if (node.argument_count() != 1) {
      throw EvaluationError(name + " requires 1 argument");
    }
    node.arguments()[0]->accept(*this);
    const XTensor& arr = EvalValueOps::to_array(pop());

    if (name == "sum") {
      stack_.push(XTensor::scalar(arr.sum_real()));
    } else if (name == "mean") {
      stack_.push(XTensor::scalar(arr.mean_real()));
    } else if (name == "min") {
      stack_.push(XTensor::scalar(arr.min_real()));
    } else if (name == "max") {
      stack_.push(XTensor::scalar(arr.max_real()));
    } else if (name == "len") {
      stack_.push(XTensor::scalar(static_cast<double>(arr.size())));
    }
    return;
  }

  // Handle element-wise math functions
  static const std::set<std::string> elementwise_funcs = {
      "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh",
      "exp", "log", "log10", "sqrt", "abs", "floor", "ceil", "round"};

  if (elementwise_funcs.count(name) && node.argument_count() == 1) {
    node.arguments()[0]->accept(*this);
    const XTensor& arr = EvalValueOps::to_array(pop());

    if (arr.is_real()) {
      const RealXTensor& a = arr.real();
      RealXTensor r;
      if (name == "sin") r = tt::sin(a);
      else if (name == "cos") r = tt::cos(a);
      else if (name == "tan") r = tt::tan(a);
      else if (name == "asin") r = tt::asin(a);
      else if (name == "acos") r = tt::acos(a);
      else if (name == "atan") r = tt::atan(a);
      else if (name == "sinh") r = tt::sinh(a);
      else if (name == "cosh") r = tt::cosh(a);
      else if (name == "tanh") r = tt::tanh(a);
      else if (name == "exp") r = tt::exp(a);
      else if (name == "log") r = tt::log(a);
      else if (name == "log10") r = tt::log10(a);
      else if (name == "sqrt") r = tt::sqrt(a);
      else if (name == "abs") r = tt::abs(a);
      else if (name == "floor") r = tt::floor(a);
      else if (name == "ceil") r = tt::ceil(a);
      else if (name == "round") r = tt::round(a);
      else throw EvaluationError("Unknown function: " + name);
      stack_.push(make_xtensor(std::move(r)));
    } else {
      const ComplexXTensor& a = arr.complex();
      if (name == "abs") {
        // abs of complex returns real array (tt::abs correctly deduces double return)
        stack_.push(make_xtensor(tt::abs(a)));
        return;
      }
      ComplexXTensor r;
      if (name == "sin") r = tt::sin(a);
      else if (name == "cos") r = tt::cos(a);
      else if (name == "tan") r = tt::tan(a);
      else if (name == "exp") r = tt::exp(a);
      else if (name == "log") r = tt::log(a);
      else if (name == "sqrt") r = tt::sqrt(a);
      else throw EvaluationError("Function " + name + " not supported for complex arrays");
      stack_.push(make_xtensor(std::move(r)));
    }
    return;
  }

  // Handle real/imag for complex arrays
  if ((name == "real" || name == "imag") && node.argument_count() == 1) {
    node.arguments()[0]->accept(*this);
    const XTensor& arr = EvalValueOps::to_array(pop());

    if (arr.is_complex()) {
      if (name == "real") {
        stack_.push(make_xtensor(detail::extract_real(arr.complex())));
      } else {
        stack_.push(make_xtensor(detail::extract_imag(arr.complex())));
      }
    } else {
      if (name == "real") {
        stack_.push(arr);
      } else {
        // imag of real tensor is zeros with same shape
        auto s = arr.shape();
        stack_.push(make_xtensor(tt::zeros<double>(tt::shape_t(s.begin(), s.end()))));
      }
    }
    return;
  }

  // Fall back to scalar evaluation for other functions
  std::vector<double> args;
  for (const auto* arg : node.arguments()) {
    arg->accept(*this);
    const XTensor& val = EvalValueOps::to_array(pop());
    if (!val.is_scalar()) {
      throw EvaluationError("Function " + name + " does not support array arguments");
    }
    args.push_back(val.get_real_at(0));
  }
  stack_.push(XTensor::scalar(functions_.evaluate_real_builtin(name, args)));
}

void ArrayEvaluator::visit(const ArrayIndex& node) {
  node.index()->accept(*this);
  const XTensor& idx_tensor = EvalValueOps::to_array(pop());
  if (!idx_tensor.is_scalar()) {
    throw EvaluationError("Array indexing with array indices not supported yet");
  }
  int idx = static_cast<int>(idx_tensor.get_real_at(0));

  node.array()->accept(*this);
  const XTensor& arr = EvalValueOps::to_array(pop());

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

void ArrayEvaluator::visit(const TernaryConditional& node) {
  node.condition()->accept(*this);
  EvalValue cond = pop();

  if (EvalValueOps::is_array(cond)) {
    throw EvaluationError("Array conditions in ternary not supported yet");
  }

  double cond_val = EvalValueOps::to_real(cond);
  if (cond_val != 0.0) {
    node.true_expr()->accept(*this);
  } else {
    node.false_expr()->accept(*this);
  }
}

}  // namespace spice_expr
