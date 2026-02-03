#include "spice_expr/ast/operations.h"

#include "spice_expr/ast/arena.h"
#include "spice_expr/visitor/visitor.h"

namespace spice_expr {

const char* binaryOpTypeToString(BinaryOpType op) {
  switch (op) {
    case BinaryOpType::Add:
      return "+";
    case BinaryOpType::Subtract:
      return "-";
    case BinaryOpType::Multiply:
      return "*";
    case BinaryOpType::Divide:
      return "/";
    case BinaryOpType::Power:
      return "**";
    case BinaryOpType::Modulo:
      return "%";
    case BinaryOpType::Equal:
      return "==";
    case BinaryOpType::NotEqual:
      return "!=";
    case BinaryOpType::Less:
      return "<";
    case BinaryOpType::LessEqual:
      return "<=";
    case BinaryOpType::Greater:
      return ">";
    case BinaryOpType::GreaterEqual:
      return ">=";
    case BinaryOpType::LogicalAnd:
      return "&&";
    case BinaryOpType::LogicalOr:
      return "||";
  }
  return "?";
}

const char* unaryOpTypeToString(UnaryOpType op) {
  switch (op) {
    case UnaryOpType::Negate:
      return "-";
    case UnaryOpType::LogicalNot:
      return "!";
    case UnaryOpType::Plus:
      return "+";
  }
  return "?";
}

// BinaryOp

void BinaryOp::accept(ExprVisitor& visitor) {
  visitor.visit(*this);
}

void BinaryOp::accept(ConstExprVisitor& visitor) const {
  visitor.visit(*this);
}

ExprNode* BinaryOp::clone(ExprArena& arena) const {
  return arena.make<BinaryOp>(op_, left_->clone(arena), right_->clone(arena));
}

size_t BinaryOp::hash() const {
  size_t h = static_cast<size_t>(op_);
  h = combineHash(h, left_->hash());
  h = combineHash(h, right_->hash());
  return h;
}

bool BinaryOp::equals(const ExprNode& other) const {
  if (other.type() != NodeType::BinaryOp)
    return false;
  const auto& o = static_cast<const BinaryOp&>(other);
  return op_ == o.op_ && left_->equals(*o.left_) && right_->equals(*o.right_);
}

// UnaryOp

void UnaryOp::accept(ExprVisitor& visitor) {
  visitor.visit(*this);
}

void UnaryOp::accept(ConstExprVisitor& visitor) const {
  visitor.visit(*this);
}

ExprNode* UnaryOp::clone(ExprArena& arena) const {
  return arena.make<UnaryOp>(op_, operand_->clone(arena));
}

size_t UnaryOp::hash() const {
  size_t h = static_cast<size_t>(op_);
  h = combineHash(h, operand_->hash());
  return h;
}

bool UnaryOp::equals(const ExprNode& other) const {
  if (other.type() != NodeType::UnaryOp)
    return false;
  const auto& o = static_cast<const UnaryOp&>(other);
  return op_ == o.op_ && operand_->equals(*o.operand_);
}

}  // namespace spice_expr
