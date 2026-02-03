#ifndef SPICE_EXPR_VISITOR_PRINTER_H
#define SPICE_EXPR_VISITOR_PRINTER_H

#include <sstream>
#include <string>

#include "spice_expr/visitor/visitor.h"

namespace spice_expr {

class ExprNode;

enum class PrintFormat { SPICE, CLike, LaTeX };

class ExprPrinter : public ConstExprVisitor {
 public:
  explicit ExprPrinter(PrintFormat format = PrintFormat::SPICE);

  std::string print(const ExprNode& node);

  void visit(const NumberLiteral& node) override;
  void visit(const StringLiteral& node) override;
  void visit(const ArrayLiteral& node) override;
  void visit(const Identifier& node) override;
  void visit(const CircuitNodeRef& node) override;
  void visit(const CircuitCurrentRef& node) override;
  void visit(const BinaryOp& node) override;
  void visit(const UnaryOp& node) override;
  void visit(const FunctionCall& node) override;
  void visit(const ArrayIndex& node) override;
  void visit(const TernaryConditional& node) override;

 private:
  static int precedence(const ExprNode& node);
  static bool needs_parens(const ExprNode& child, const ExprNode& parent, bool isRight);

  PrintFormat format_;
  std::ostringstream ss_;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_VISITOR_PRINTER_H
