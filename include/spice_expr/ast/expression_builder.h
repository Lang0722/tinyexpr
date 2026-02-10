#ifndef SPICE_EXPR_AST_EXPRESSION_BUILDER_H
#define SPICE_EXPR_AST_EXPRESSION_BUILDER_H

#include <string>
#include <vector>

#include "spice_expr/ast/expr_node.h"
#include "spice_expr/ast/operations.h"

namespace spice_expr {

class ExprArena;

class ExpressionBuilder {
 public:
  explicit ExpressionBuilder(ExprArena& arena);

  // Operand pushers (push 1)
  ExpressionBuilder& num(double value);
  ExpressionBuilder& complex(double real, double imag);
  ExpressionBuilder& str(std::string value);
  ExpressionBuilder& id(std::string name);
  ExpressionBuilder& voltage(std::string node);
  ExpressionBuilder& voltage(std::string node1, std::string node2);
  ExpressionBuilder& current(std::string device);
  ExpressionBuilder& push(ExprNode* node);

  // Binary operators (pop 2, push 1)
  ExpressionBuilder& add();
  ExpressionBuilder& sub();
  ExpressionBuilder& mul();
  ExpressionBuilder& div();
  ExpressionBuilder& pow();
  ExpressionBuilder& mod();
  ExpressionBuilder& eq();
  ExpressionBuilder& ne();
  ExpressionBuilder& lt();
  ExpressionBuilder& le();
  ExpressionBuilder& gt();
  ExpressionBuilder& ge();
  ExpressionBuilder& land();
  ExpressionBuilder& lor();

  // Unary operators (pop 1, push 1)
  ExpressionBuilder& neg();
  ExpressionBuilder& lnot();

  // Functions and compound (pop n, push 1)
  ExpressionBuilder& call(std::string name, int n);
  ExpressionBuilder& call1(std::string name);
  ExpressionBuilder& call2(std::string name);
  ExpressionBuilder& index();
  ExpressionBuilder& ternary();
  ExpressionBuilder& array(int n);

  // Result/control
  ExprNode* result() const;
  ExprNode* build();
  void clear();
  int depth() const;

 private:
  ExprNode* pop();
  void push_node(ExprNode* node);
  ExpressionBuilder& binary_op(BinaryOpType op);
  ExpressionBuilder& unary_op(UnaryOpType op);

  ExprArena& arena_;
  std::vector<ExprNode*> stack_;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_AST_EXPRESSION_BUILDER_H
