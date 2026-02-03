#ifndef SPICE_EXPR_AST_LITERALS_H
#define SPICE_EXPR_AST_LITERALS_H

#include <complex>
#include <functional>
#include <string>
#include <vector>

#include "spice_expr/ast/expr_node.h"

namespace spice_expr {

class ExprArena;
class ExprVisitor;
class ConstExprVisitor;

class NumberLiteral : public ExprNode {
 public:
  explicit NumberLiteral(double value)
      : ExprNode(NodeType::NumberLiteral), real_(value), imag_(0.0), is_complex_(false) {}

  NumberLiteral(double real, double imag)
      : ExprNode(NodeType::NumberLiteral), real_(real), imag_(imag), is_complex_(true) {}

  double real() const { return real_; }
  double imag() const { return imag_; }
  bool is_complex() const { return is_complex_; }
  std::complex<double> complex_value() const { return {real_, imag_}; }

  void accept(ExprVisitor& visitor) override;
  void accept(ConstExprVisitor& visitor) const override;
  ExprNode* clone(ExprArena& arena) const override;
  size_t hash() const override;
  bool equals(const ExprNode& other) const override;

 private:
  double real_;
  double imag_;
  bool is_complex_;
};

class StringLiteral : public ExprNode {
 public:
  explicit StringLiteral(std::string value)
      : ExprNode(NodeType::StringLiteral), value_(std::move(value)) {}

  const std::string& value() const { return value_; }

  void accept(ExprVisitor& visitor) override;
  void accept(ConstExprVisitor& visitor) const override;
  ExprNode* clone(ExprArena& arena) const override;
  size_t hash() const override;
  bool equals(const ExprNode& other) const override;

 private:
  std::string value_;
};

class ArrayLiteral : public ExprNode {
 public:
  explicit ArrayLiteral(std::vector<ExprNode*> elements)
      : ExprNode(NodeType::ArrayLiteral), elements_(std::move(elements)) {}

  const std::vector<ExprNode*>& elements() const { return elements_; }
  size_t element_count() const { return elements_.size(); }

  void accept(ExprVisitor& visitor) override;
  void accept(ConstExprVisitor& visitor) const override;
  ExprNode* clone(ExprArena& arena) const override;
  size_t hash() const override;
  bool equals(const ExprNode& other) const override;

 private:
  std::vector<ExprNode*> elements_;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_AST_LITERALS_H
