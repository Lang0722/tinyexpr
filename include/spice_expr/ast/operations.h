#ifndef SPICE_EXPR_AST_OPERATIONS_H
#define SPICE_EXPR_AST_OPERATIONS_H

#include "spice_expr/ast/expr_node.h"

namespace spice_expr {

class ExprArena;
class ExprVisitor;
class ConstExprVisitor;

enum class BinaryOpType : uint8_t {
  Add,
  Subtract,
  Multiply,
  Divide,
  Power,
  Modulo,
  Equal,
  NotEqual,
  Less,
  LessEqual,
  Greater,
  GreaterEqual,
  LogicalAnd,
  LogicalOr
};

enum class UnaryOpType : uint8_t { Negate, LogicalNot, Plus };

const char* binaryOpTypeToString(BinaryOpType op);
const char* unaryOpTypeToString(UnaryOpType op);

class BinaryOp : public ExprNode {
 public:
  BinaryOp(BinaryOpType op, ExprNode* left, ExprNode* right)
      : ExprNode(NodeType::BinaryOp), op_(op), left_(left), right_(right) {}

  BinaryOpType op_type() const { return op_; }
  ExprNode* left() const { return left_; }
  ExprNode* right() const { return right_; }

  void accept(ExprVisitor& visitor) override;
  void accept(ConstExprVisitor& visitor) const override;
  ExprNode* clone(ExprArena& arena) const override;
  size_t hash() const override;
  bool equals(const ExprNode& other) const override;

 private:
  BinaryOpType op_;
  ExprNode* left_;
  ExprNode* right_;
};

class UnaryOp : public ExprNode {
 public:
  UnaryOp(UnaryOpType op, ExprNode* operand)
      : ExprNode(NodeType::UnaryOp), op_(op), operand_(operand) {}

  UnaryOpType op_type() const { return op_; }
  ExprNode* operand() const { return operand_; }

  void accept(ExprVisitor& visitor) override;
  void accept(ConstExprVisitor& visitor) const override;
  ExprNode* clone(ExprArena& arena) const override;
  size_t hash() const override;
  bool equals(const ExprNode& other) const override;

 private:
  UnaryOpType op_;
  ExprNode* operand_;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_AST_OPERATIONS_H
