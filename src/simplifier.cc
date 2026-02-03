#include "spice_expr/visitor/simplifier.h"

#include <cmath>
#include <complex>
#include <vector>

#include "spice_expr/ast/arena.h"
#include "spice_expr/ast/functions.h"
#include "spice_expr/ast/literals.h"
#include "spice_expr/ast/operations.h"
#include "spice_expr/ast/references.h"
#include "spice_expr/symbol/symbol_table.h"

namespace spice_expr {

Simplifier::Simplifier(ExprArena& arena, const SymbolTable* symbols)
    : arena_(arena), symbols_(symbols), result_(nullptr) {}

ExprNode* Simplifier::simplify(const ExprNode& node) {
  result_ = nullptr;
  node.accept(*this);
  return result_;
}

ExprNode* Simplifier::simplify_to_fixpoint(const ExprNode& node, int maxIterations) {
  ExprNode* current = const_cast<ExprNode*>(&node);
  for (int i = 0; i < maxIterations; ++i) {
    ExprNode* simplified = simplify(*current);
    if (simplified->equals(*current)) {
      return simplified;
    }
    current = simplified;
  }
  return current;
}

void Simplifier::visit(const NumberLiteral& node) {
  result_ = node.clone(arena_);
}

void Simplifier::visit(const StringLiteral& node) {
  result_ = node.clone(arena_);
}

void Simplifier::visit(const ArrayLiteral& node) {
  std::vector<ExprNode*> simplified;
  simplified.reserve(node.element_count());
  for (const auto* elem : node.elements()) {
    simplified.push_back(simplify(*elem));
  }
  result_ = arena_.make<ArrayLiteral>(std::move(simplified));
}

void Simplifier::visit(const Identifier& node) {
  result_ = node.clone(arena_);
}

void Simplifier::visit(const CircuitNodeRef& node) {
  result_ = node.clone(arena_);
}

void Simplifier::visit(const CircuitCurrentRef& node) {
  result_ = node.clone(arena_);
}

void Simplifier::visit(const BinaryOp& node) {
  ExprNode* left = simplify(*node.left());
  ExprNode* right = simplify(*node.right());

  // Commutativity exploitation: flatten nested associative ops and fold constants
  if (is_commutative_associative(node.op_type())) {
    bool left_same = (left->type() == NodeType::BinaryOp &&
                      static_cast<const BinaryOp*>(left)->op_type() == node.op_type());
    bool right_same = (right->type() == NodeType::BinaryOp &&
                       static_cast<const BinaryOp*>(right)->op_type() == node.op_type());

    if (left_same || right_same) {
      std::vector<ExprNode*> operands;
      collect_operands(*left, node.op_type(), operands);
      collect_operands(*right, node.op_type(), operands);

      // Only proceed if there are multiple constants to fold
      int constant_count = 0;
      for (const auto* op : operands) {
        if (is_constant(*op)) ++constant_count;
      }

      if (constant_count >= 2) {
        result_ = fold_constants(operands, node.op_type());
        return;
      }
    }
  }

  if (is_constant(*left) && is_constant(*right)) {
    // Use complex arithmetic if either operand is complex
    if (is_complex_constant(*left) || is_complex_constant(*right)) {
      auto l = get_complex_value(*left);
      auto r = get_complex_value(*right);
      std::complex<double> result;
      bool valid = true;

      switch (node.op_type()) {
        case BinaryOpType::Add:
          result = l + r;
          break;
        case BinaryOpType::Subtract:
          result = l - r;
          break;
        case BinaryOpType::Multiply:
          result = l * r;
          break;
        case BinaryOpType::Divide:
          if (r != std::complex<double>(0.0, 0.0))
            result = l / r;
          else
            valid = false;
          break;
        case BinaryOpType::Power:
          result = std::pow(l, r);
          break;
        case BinaryOpType::Equal:
          result = (l == r) ? std::complex<double>(1.0, 0.0) : std::complex<double>(0.0, 0.0);
          break;
        case BinaryOpType::NotEqual:
          result = (l != r) ? std::complex<double>(1.0, 0.0) : std::complex<double>(0.0, 0.0);
          break;
        // Comparison and logical ops not well-defined for complex - skip folding
        case BinaryOpType::Modulo:
        case BinaryOpType::Less:
        case BinaryOpType::LessEqual:
        case BinaryOpType::Greater:
        case BinaryOpType::GreaterEqual:
        case BinaryOpType::LogicalAnd:
        case BinaryOpType::LogicalOr:
          valid = false;
          break;
        default:
          valid = false;
      }

      if (valid) {
        result_ = arena_.make<NumberLiteral>(result.real(), result.imag());
        return;
      }
    } else {
      // Both operands are real constants
      double l = get_constant_value(*left);
      double r = get_constant_value(*right);
      double result;
      bool valid = true;

      switch (node.op_type()) {
        case BinaryOpType::Add:
          result = l + r;
          break;
        case BinaryOpType::Subtract:
          result = l - r;
          break;
        case BinaryOpType::Multiply:
          result = l * r;
          break;
        case BinaryOpType::Divide:
          if (r != 0)
            result = l / r;
          else
            valid = false;
          break;
        case BinaryOpType::Power:
          result = std::pow(l, r);
          break;
        case BinaryOpType::Modulo:
          if (r != 0)
            result = std::fmod(l, r);
          else
            valid = false;
          break;
        case BinaryOpType::Equal:
          result = (l == r) ? 1.0 : 0.0;
          break;
        case BinaryOpType::NotEqual:
          result = (l != r) ? 1.0 : 0.0;
          break;
        case BinaryOpType::Less:
          result = (l < r) ? 1.0 : 0.0;
          break;
        case BinaryOpType::LessEqual:
          result = (l <= r) ? 1.0 : 0.0;
          break;
        case BinaryOpType::Greater:
          result = (l > r) ? 1.0 : 0.0;
          break;
        case BinaryOpType::GreaterEqual:
          result = (l >= r) ? 1.0 : 0.0;
          break;
        case BinaryOpType::LogicalAnd:
          result = (l && r) ? 1.0 : 0.0;
          break;
        case BinaryOpType::LogicalOr:
          result = (l || r) ? 1.0 : 0.0;
          break;
        default:
          valid = false;
      }

      if (valid) {
        result_ = arena_.make<NumberLiteral>(result);
        return;
      }
    }
  }

  switch (node.op_type()) {
    case BinaryOpType::Add:
      if (is_zero(*left)) {
        result_ = right;
        return;
      }
      if (is_zero(*right)) {
        result_ = left;
        return;
      }
      // x + x = 2 * x
      if (left->equals(*right)) {
        result_ = arena_.make<BinaryOp>(BinaryOpType::Multiply,
                                        arena_.make<NumberLiteral>(2.0), left);
        return;
      }
      break;

    case BinaryOpType::Subtract:
      if (is_zero(*right)) {
        result_ = left;
        return;
      }
      if (left->equals(*right)) {
        result_ = arena_.make<NumberLiteral>(0.0);
        return;
      }
      break;

    case BinaryOpType::Multiply:
      if (is_zero(*left) || is_zero(*right)) {
        result_ = arena_.make<NumberLiteral>(0.0);
        return;
      }
      if (is_one(*left)) {
        result_ = right;
        return;
      }
      if (is_one(*right)) {
        result_ = left;
        return;
      }
      // x * x = x ^ 2
      if (left->equals(*right)) {
        result_ = arena_.make<BinaryOp>(BinaryOpType::Power, left,
                                        arena_.make<NumberLiteral>(2.0));
        return;
      }
      break;

    case BinaryOpType::Divide:
      if (is_zero(*left)) {
        result_ = arena_.make<NumberLiteral>(0.0);
        return;
      }
      if (is_one(*right)) {
        result_ = left;
        return;
      }
      if (left->equals(*right)) {
        result_ = arena_.make<NumberLiteral>(1.0);
        return;
      }
      break;

    case BinaryOpType::Power:
      // 0^0 is mathematically undefined - leave unsimplified
      if (is_zero(*left) && is_zero(*right)) {
        break;  // fall through to create BinaryOp
      }
      if (is_zero(*right)) {
        result_ = arena_.make<NumberLiteral>(1.0);
        return;
      }
      if (is_one(*right)) {
        result_ = left;
        return;
      }
      if (is_zero(*left)) {
        result_ = arena_.make<NumberLiteral>(0.0);
        return;
      }
      break;

    default:
      break;
  }

  result_ = arena_.make<BinaryOp>(node.op_type(), left, right);
}

void Simplifier::visit(const UnaryOp& node) {
  ExprNode* operand = simplify(*node.operand());

  if (is_constant(*operand)) {
    // Use complex arithmetic to preserve imaginary part
    if (is_complex_constant(*operand)) {
      auto val = get_complex_value(*operand);
      std::complex<double> result;

      switch (node.op_type()) {
        case UnaryOpType::Negate:
          result = -val;
          break;
        case UnaryOpType::Plus:
          result = val;
          break;
        case UnaryOpType::LogicalNot:
          // LogicalNot checks if the entire complex value is zero
          result = (val == std::complex<double>(0.0, 0.0)) ? 1.0 : 0.0;
          break;
        default:
          result = val;
      }

      result_ = arena_.make<NumberLiteral>(result.real(), result.imag());
      return;
    } else {
      // Real constant - use simpler path
      double val = get_constant_value(*operand);
      double result;

      switch (node.op_type()) {
        case UnaryOpType::Negate:
          result = -val;
          break;
        case UnaryOpType::Plus:
          result = val;
          break;
        case UnaryOpType::LogicalNot:
          result = (val == 0) ? 1.0 : 0.0;
          break;
        default:
          result = val;
      }

      result_ = arena_.make<NumberLiteral>(result);
      return;
    }
  }

  if (node.op_type() == UnaryOpType::Plus) {
    result_ = operand;
    return;
  }

  if (node.op_type() == UnaryOpType::Negate && operand->type() == NodeType::UnaryOp) {
    auto& inner = static_cast<const UnaryOp&>(*operand);
    if (inner.op_type() == UnaryOpType::Negate) {
      result_ = inner.operand()->clone(arena_);
      return;
    }
  }

  result_ = arena_.make<UnaryOp>(node.op_type(), operand);
}

void Simplifier::visit(const FunctionCall& node) {
  std::vector<ExprNode*> simplified;
  simplified.reserve(node.argument_count());
  for (const auto* arg : node.arguments()) {
    simplified.push_back(simplify(*arg));
  }
  result_ = arena_.make<FunctionCall>(node.name(), std::move(simplified));
}

void Simplifier::visit(const ArrayIndex& node) {
  ExprNode* array = simplify(*node.array());
  ExprNode* index = simplify(*node.index());
  result_ = arena_.make<ArrayIndex>(array, index);
}

void Simplifier::visit(const TernaryConditional& node) {
  ExprNode* cond = simplify(*node.condition());

  if (is_constant(*cond)) {
    // Check if condition is non-zero (considering both real and imaginary parts)
    bool is_truthy = !is_zero(*cond);
    if (is_truthy) {
      result_ = simplify(*node.true_expr());
    } else {
      result_ = simplify(*node.false_expr());
    }
    return;
  }

  result_ = arena_.make<TernaryConditional>(cond, simplify(*node.true_expr()),
                                            simplify(*node.false_expr()));
}

bool Simplifier::is_zero(const ExprNode& node) const {
  if (!is_constant(node)) return false;
  const auto& lit = static_cast<const NumberLiteral&>(node);
  return lit.real() == 0.0 && lit.imag() == 0.0;
}

bool Simplifier::is_one(const ExprNode& node) const {
  if (!is_constant(node)) return false;
  const auto& lit = static_cast<const NumberLiteral&>(node);
  return lit.real() == 1.0 && lit.imag() == 0.0;
}

bool Simplifier::is_constant(const ExprNode& node) const {
  return node.type() == NodeType::NumberLiteral;
}

bool Simplifier::is_complex_constant(const ExprNode& node) const {
  if (node.type() == NodeType::NumberLiteral) {
    return static_cast<const NumberLiteral&>(node).is_complex();
  }
  return false;
}

double Simplifier::get_constant_value(const ExprNode& node) const {
  if (node.type() == NodeType::NumberLiteral) {
    return static_cast<const NumberLiteral&>(node).real();
  }
  return 0.0;
}

std::complex<double> Simplifier::get_complex_value(const ExprNode& node) const {
  if (node.type() == NodeType::NumberLiteral) {
    return static_cast<const NumberLiteral&>(node).complex_value();
  }
  return {0.0, 0.0};
}

bool Simplifier::is_commutative_associative(BinaryOpType op) const {
  return op == BinaryOpType::Add || op == BinaryOpType::Multiply;
}

void Simplifier::collect_operands(const ExprNode& node, BinaryOpType op,
                                  std::vector<ExprNode*>& operands) {
  if (node.type() == NodeType::BinaryOp) {
    const auto& binop = static_cast<const BinaryOp&>(node);
    if (binop.op_type() == op) {
      collect_operands(*binop.left(), op, operands);
      collect_operands(*binop.right(), op, operands);
      return;
    }
  }
  // Not the same op type or not a BinaryOp - this is a leaf operand
  operands.push_back(const_cast<ExprNode*>(&node));
}

ExprNode* Simplifier::fold_constants(const std::vector<ExprNode*>& operands,
                                     BinaryOpType op) {
  std::vector<ExprNode*> non_constants;
  double accumulated = (op == BinaryOpType::Add) ? 0.0 : 1.0;
  std::complex<double> accumulated_complex =
      (op == BinaryOpType::Add) ? std::complex<double>(0.0, 0.0)
                                : std::complex<double>(1.0, 0.0);
  bool has_complex = false;
  bool has_constant = false;

  for (ExprNode* operand : operands) {
    if (is_constant(*operand)) {
      has_constant = true;
      if (is_complex_constant(*operand)) {
        has_complex = true;
        auto val = get_complex_value(*operand);
        if (op == BinaryOpType::Add) {
          accumulated_complex += val;
        } else {
          accumulated_complex *= val;
        }
      } else {
        double val = get_constant_value(*operand);
        if (op == BinaryOpType::Add) {
          accumulated += val;
          accumulated_complex += std::complex<double>(val, 0.0);
        } else {
          accumulated *= val;
          accumulated_complex *= std::complex<double>(val, 0.0);
        }
      }
    } else {
      non_constants.push_back(operand);
    }
  }

  // Build constant node if we have constants
  ExprNode* constant_node = nullptr;
  if (has_constant) {
    if (has_complex) {
      constant_node = arena_.make<NumberLiteral>(accumulated_complex.real(),
                                                 accumulated_complex.imag());
    } else {
      constant_node = arena_.make<NumberLiteral>(accumulated);
    }
  }

  // If all constants, return the folded value
  if (non_constants.empty()) {
    return constant_node;
  }

  // Build tree from non-constants
  ExprNode* result = non_constants[0];
  for (size_t i = 1; i < non_constants.size(); ++i) {
    result = arena_.make<BinaryOp>(op, result, non_constants[i]);
  }

  // Add the constant if not identity (0 for Add, 1 for Multiply)
  if (constant_node) {
    bool is_identity = false;
    if (op == BinaryOpType::Add) {
      is_identity = is_zero(*constant_node);
    } else if (op == BinaryOpType::Multiply) {
      is_identity = is_one(*constant_node);
    }

    if (!is_identity) {
      result = arena_.make<BinaryOp>(op, result, constant_node);
    }
  }

  return result;
}

}  // namespace spice_expr
