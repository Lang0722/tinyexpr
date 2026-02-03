#ifndef SPICE_EXPR_AST_REFERENCES_H
#define SPICE_EXPR_AST_REFERENCES_H

#include <string>

#include "spice_expr/ast/expr_node.h"

namespace spice_expr {

class ExprArena;
class ExprVisitor;
class ConstExprVisitor;

class Identifier : public ExprNode {
 public:
  explicit Identifier(std::string name) : ExprNode(NodeType::Identifier), name_(std::move(name)) {}

  const std::string& name() const { return name_; }

  void accept(ExprVisitor& visitor) override;
  void accept(ConstExprVisitor& visitor) const override;
  ExprNode* clone(ExprArena& arena) const override;
  size_t hash() const override;
  bool equals(const ExprNode& other) const override;

 private:
  std::string name_;
};

class CircuitNodeRef : public ExprNode {
 public:
  explicit CircuitNodeRef(std::string node)
      : ExprNode(NodeType::CircuitNodeRef), node1_(std::move(node)), differential_(false) {}

  CircuitNodeRef(std::string node1, std::string node2)
      : ExprNode(NodeType::CircuitNodeRef),
        node1_(std::move(node1)),
        node2_(std::move(node2)),
        differential_(true) {}

  const std::string& node1() const { return node1_; }
  const std::string& node2() const { return node2_; }
  bool is_differential() const { return differential_; }

  void accept(ExprVisitor& visitor) override;
  void accept(ConstExprVisitor& visitor) const override;
  ExprNode* clone(ExprArena& arena) const override;
  size_t hash() const override;
  bool equals(const ExprNode& other) const override;

 private:
  std::string node1_;
  std::string node2_;
  bool differential_;
};

class CircuitCurrentRef : public ExprNode {
 public:
  explicit CircuitCurrentRef(std::string device)
      : ExprNode(NodeType::CircuitCurrentRef), device_(std::move(device)) {}

  const std::string& device() const { return device_; }

  void accept(ExprVisitor& visitor) override;
  void accept(ConstExprVisitor& visitor) const override;
  ExprNode* clone(ExprArena& arena) const override;
  size_t hash() const override;
  bool equals(const ExprNode& other) const override;

 private:
  std::string device_;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_AST_REFERENCES_H
