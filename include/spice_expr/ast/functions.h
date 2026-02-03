#ifndef SPICE_EXPR_AST_FUNCTIONS_H
#define SPICE_EXPR_AST_FUNCTIONS_H

#include <string>
#include <vector>

#include "spice_expr/ast/expr_node.h"

namespace spice_expr {

class ExprArena;
class ExprVisitor;
class ConstExprVisitor;

class FunctionCall : public ExprNode {
 public:
  FunctionCall(std::string name, std::vector<ExprNode*> arguments)
      : ExprNode(NodeType::FunctionCall),
        name_(std::move(name)),
        arguments_(std::move(arguments)) {}

  const std::string& name() const { return name_; }
  const std::vector<ExprNode*>& arguments() const { return arguments_; }
  size_t argument_count() const { return arguments_.size(); }

  void accept(ExprVisitor& visitor) override;
  void accept(ConstExprVisitor& visitor) const override;
  ExprNode* clone(ExprArena& arena) const override;
  size_t hash() const override;
  bool equals(const ExprNode& other) const override;

 private:
  std::string name_;
  std::vector<ExprNode*> arguments_;
};

class ArrayIndex : public ExprNode {
 public:
  ArrayIndex(ExprNode* array, ExprNode* index)
      : ExprNode(NodeType::ArrayIndex), array_(array), index_(index) {}

  ExprNode* array() const { return array_; }
  ExprNode* index() const { return index_; }

  void accept(ExprVisitor& visitor) override;
  void accept(ConstExprVisitor& visitor) const override;
  ExprNode* clone(ExprArena& arena) const override;
  size_t hash() const override;
  bool equals(const ExprNode& other) const override;

 private:
  ExprNode* array_;
  ExprNode* index_;
};

class TernaryConditional : public ExprNode {
 public:
  TernaryConditional(ExprNode* condition, ExprNode* trueExpr, ExprNode* falseExpr)
      : ExprNode(NodeType::TernaryConditional),
        condition_(condition),
        trueExpr_(trueExpr),
        falseExpr_(falseExpr) {}

  ExprNode* condition() const { return condition_; }
  ExprNode* true_expr() const { return trueExpr_; }
  ExprNode* false_expr() const { return falseExpr_; }

  void accept(ExprVisitor& visitor) override;
  void accept(ConstExprVisitor& visitor) const override;
  ExprNode* clone(ExprArena& arena) const override;
  size_t hash() const override;
  bool equals(const ExprNode& other) const override;

 private:
  ExprNode* condition_;
  ExprNode* trueExpr_;
  ExprNode* falseExpr_;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_AST_FUNCTIONS_H
