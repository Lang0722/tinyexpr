#include "spice_expr/visitor/differentiator.h"

#include <stdexcept>

#include "spice_expr/ast/arena.h"
#include "spice_expr/ast/functions.h"
#include "spice_expr/ast/literals.h"
#include "spice_expr/ast/operations.h"
#include "spice_expr/ast/references.h"
#include "spice_expr/symbol/symbol_table.h"

namespace spice_expr {

const std::string& DiffTarget::name() const {
  if (is_parameter()) {
    return std::get<Parameter>(target_).name;
  } else if (is_node_voltage()) {
    return std::get<NodeVoltage>(target_).node;
  } else {
    return std::get<DeviceCurrent>(target_).device;
  }
}

Differentiator::Differentiator(ExprArena& arena, DiffTarget target)
    : arena_(arena), target_(std::move(target)), result_(nullptr), symbols_(nullptr) {}

Differentiator::Differentiator(ExprArena& arena, DiffTarget target, const SymbolTable* symbols)
    : arena_(arena), target_(std::move(target)), result_(nullptr), symbols_(symbols) {}

ExprNode* Differentiator::differentiate(const ExprNode& node) {
  result_ = nullptr;
  node.accept(*this);
  return result_;
}

ExprNode* Differentiator::zero() {
  return arena_.make<NumberLiteral>(0.0);
}

ExprNode* Differentiator::one() {
  return arena_.make<NumberLiteral>(1.0);
}

ExprNode* Differentiator::clone_node(const ExprNode& node) {
  return node.clone(arena_);
}

void Differentiator::visit(const NumberLiteral&) {
  result_ = zero();
}

void Differentiator::visit(const StringLiteral&) {
  result_ = zero();
}

void Differentiator::visit(const ArrayLiteral&) {
  throw std::runtime_error("Cannot differentiate array literal");
}

void Differentiator::visit(const Identifier& node) {
  // Direct match: d(x)/dx = 1
  if (target_.is_parameter() && node.name() == target_.name()) {
    result_ = one();
    return;
  }

  // No symbol table → no chain rule (backward compatible)
  if (!symbols_) {
    result_ = zero();
    return;
  }

  // Cycle detection
  if (visiting_.count(node.name()) > 0) {
    throw std::runtime_error("Circular dependency in differentiation: " + node.name());
  }

  // Lookup identifier's definition
  const SymbolEntry* entry = symbols_->lookup(node.name());
  if (!entry || !entry->expression) {
    result_ = zero();  // Unknown or constant parameter
    return;
  }

  // Apply chain rule: differentiate the definition
  visiting_.insert(node.name());
  result_ = differentiate(*entry->expression);
  visiting_.erase(node.name());
}

void Differentiator::visit(const CircuitNodeRef& node) {
  if (target_.is_node_voltage()) {
    if (!node.is_differential() && node.node1() == target_.name()) {
      result_ = one();
      return;
    }
    if (node.is_differential()) {
      bool match1 = (node.node1() == target_.name());
      bool match2 = (node.node2() == target_.name());
      if (match1 && !match2) {
        result_ = one();
        return;
      }
      if (!match1 && match2) {
        result_ = arena_.make<NumberLiteral>(-1.0);
        return;
      }
      if (match1 && match2) {
        result_ = zero();
        return;
      }
    }
  }
  result_ = zero();
}

void Differentiator::visit(const CircuitCurrentRef& node) {
  if (target_.is_device_current() && node.device() == target_.name()) {
    result_ = one();
  } else {
    result_ = zero();
  }
}

void Differentiator::visit(const BinaryOp& node) {
  ExprNode* leftDeriv = differentiate(*node.left());
  ExprNode* rightDeriv = differentiate(*node.right());
  ExprNode* left = clone_node(*node.left());
  ExprNode* right = clone_node(*node.right());

  switch (node.op_type()) {
    case BinaryOpType::Add:
      result_ = arena_.make<BinaryOp>(BinaryOpType::Add, leftDeriv, rightDeriv);
      break;

    case BinaryOpType::Subtract:
      result_ = arena_.make<BinaryOp>(BinaryOpType::Subtract, leftDeriv, rightDeriv);
      break;

    case BinaryOpType::Multiply:
      result_ = arena_.make<BinaryOp>(
          BinaryOpType::Add, arena_.make<BinaryOp>(BinaryOpType::Multiply, leftDeriv, right),
          arena_.make<BinaryOp>(BinaryOpType::Multiply, left, rightDeriv));
      break;

    case BinaryOpType::Divide:
      result_ = arena_.make<BinaryOp>(
          BinaryOpType::Divide,
          arena_.make<BinaryOp>(
              BinaryOpType::Subtract,
              arena_.make<BinaryOp>(BinaryOpType::Multiply, leftDeriv, clone_node(*node.right())),
              arena_.make<BinaryOp>(BinaryOpType::Multiply, clone_node(*node.left()), rightDeriv)),
          arena_.make<BinaryOp>(BinaryOpType::Power, right, arena_.make<NumberLiteral>(2.0)));
      break;

    case BinaryOpType::Power:
      if (node.right()->type() == NodeType::NumberLiteral) {
        double exp = static_cast<const NumberLiteral&>(*node.right()).real();
        result_ = arena_.make<BinaryOp>(
            BinaryOpType::Multiply,
            arena_.make<BinaryOp>(BinaryOpType::Multiply, arena_.make<NumberLiteral>(exp),
                                  arena_.make<BinaryOp>(BinaryOpType::Power, left,
                                                        arena_.make<NumberLiteral>(exp - 1))),
            leftDeriv);
      } else {
        ExprNode* lnLeft =
            arena_.make<FunctionCall>("ln", std::vector<ExprNode*>{clone_node(*node.left())});
        ExprNode* term1 = arena_.make<BinaryOp>(BinaryOpType::Multiply, rightDeriv, lnLeft);
        ExprNode* term2 = arena_.make<BinaryOp>(
            BinaryOpType::Divide, arena_.make<BinaryOp>(BinaryOpType::Multiply, right, leftDeriv),
            clone_node(*node.left()));
        result_ = arena_.make<BinaryOp>(
            BinaryOpType::Multiply,
            arena_.make<BinaryOp>(BinaryOpType::Power, left, clone_node(*node.right())),
            arena_.make<BinaryOp>(BinaryOpType::Add, term1, term2));
      }
      break;

    default:
      throw std::runtime_error("Differentiation not supported for this operator");
  }
}

void Differentiator::visit(const UnaryOp& node) {
  ExprNode* operandDeriv = differentiate(*node.operand());

  switch (node.op_type()) {
    case UnaryOpType::Negate:
      result_ = arena_.make<UnaryOp>(UnaryOpType::Negate, operandDeriv);
      break;

    case UnaryOpType::Plus:
      result_ = operandDeriv;
      break;

    default:
      throw std::runtime_error("Differentiation not supported for this unary operator");
  }
}

void Differentiator::visit(const FunctionCall& node) {
  if (node.argument_count() != 1) {
    throw std::runtime_error("Differentiation only supported for single-argument functions");
  }

  ExprNode* arg = clone_node(*node.arguments()[0]);
  ExprNode* argDeriv = differentiate(*node.arguments()[0]);
  std::string name = node.name();

  ExprNode* funcDeriv = nullptr;

  if (name == "sin") {
    funcDeriv = arena_.make<FunctionCall>("cos", std::vector<ExprNode*>{arg});
  } else if (name == "cos") {
    funcDeriv = arena_.make<UnaryOp>(UnaryOpType::Negate,
                                     arena_.make<FunctionCall>("sin", std::vector<ExprNode*>{arg}));
  } else if (name == "tan") {
    ExprNode* cos = arena_.make<FunctionCall>("cos", std::vector<ExprNode*>{clone_node(*arg)});
    funcDeriv = arena_.make<BinaryOp>(
        BinaryOpType::Divide, one(),
        arena_.make<BinaryOp>(BinaryOpType::Power, cos, arena_.make<NumberLiteral>(2.0)));
  } else if (name == "exp") {
    funcDeriv = arena_.make<FunctionCall>("exp", std::vector<ExprNode*>{arg});
  } else if (name == "log" || name == "ln") {
    funcDeriv = arena_.make<BinaryOp>(BinaryOpType::Divide, one(), arg);
  } else if (name == "log10") {
    funcDeriv = arena_.make<BinaryOp>(
        BinaryOpType::Divide, one(),
        arena_.make<BinaryOp>(BinaryOpType::Multiply, arg,
                              arena_.make<FunctionCall>(
                                  "ln", std::vector<ExprNode*>{arena_.make<NumberLiteral>(10.0)})));
  } else if (name == "sqrt") {
    funcDeriv = arena_.make<BinaryOp>(
        BinaryOpType::Divide, one(),
        arena_.make<BinaryOp>(BinaryOpType::Multiply, arena_.make<NumberLiteral>(2.0),
                              arena_.make<FunctionCall>("sqrt", std::vector<ExprNode*>{arg})));
  } else if (name == "asin") {
    funcDeriv = arena_.make<BinaryOp>(
        BinaryOpType::Divide, one(),
        arena_.make<FunctionCall>("sqrt",
                                  std::vector<ExprNode*>{arena_.make<BinaryOp>(
                                      BinaryOpType::Subtract, one(),
                                      arena_.make<BinaryOp>(BinaryOpType::Power, arg,
                                                            arena_.make<NumberLiteral>(2.0)))}));
  } else if (name == "acos") {
    funcDeriv = arena_.make<UnaryOp>(
        UnaryOpType::Negate,
        arena_.make<BinaryOp>(
            BinaryOpType::Divide, one(),
            arena_.make<FunctionCall>(
                "sqrt", std::vector<ExprNode*>{arena_.make<BinaryOp>(
                            BinaryOpType::Subtract, one(),
                            arena_.make<BinaryOp>(BinaryOpType::Power, clone_node(*arg),
                                                  arena_.make<NumberLiteral>(2.0)))})));
  } else if (name == "atan") {
    funcDeriv = arena_.make<BinaryOp>(
        BinaryOpType::Divide, one(),
        arena_.make<BinaryOp>(
            BinaryOpType::Add, one(),
            arena_.make<BinaryOp>(BinaryOpType::Power, arg, arena_.make<NumberLiteral>(2.0))));
  } else if (name == "sinh") {
    funcDeriv = arena_.make<FunctionCall>("cosh", std::vector<ExprNode*>{arg});
  } else if (name == "cosh") {
    funcDeriv = arena_.make<FunctionCall>("sinh", std::vector<ExprNode*>{arg});
  } else if (name == "tanh") {
    ExprNode* cosh = arena_.make<FunctionCall>("cosh", std::vector<ExprNode*>{clone_node(*arg)});
    funcDeriv = arena_.make<BinaryOp>(
        BinaryOpType::Divide, one(),
        arena_.make<BinaryOp>(BinaryOpType::Power, cosh, arena_.make<NumberLiteral>(2.0)));
  } else if (name == "abs") {
    funcDeriv = arena_.make<FunctionCall>("sgn", std::vector<ExprNode*>{arg});
  } else {
    throw std::runtime_error("Differentiation not supported for function: " + name);
  }

  result_ = arena_.make<BinaryOp>(BinaryOpType::Multiply, funcDeriv, argDeriv);
}

void Differentiator::visit(const ArrayIndex&) {
  throw std::runtime_error("Cannot differentiate array index expression");
}

void Differentiator::visit(const TernaryConditional&) {
  throw std::runtime_error("Cannot differentiate ternary conditional expression");
}

}  // namespace spice_expr
