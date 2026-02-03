#ifndef SPICE_EXPR_VISITOR_VISITOR_H
#define SPICE_EXPR_VISITOR_VISITOR_H

namespace spice_expr {

class NumberLiteral;
class StringLiteral;
class ArrayLiteral;
class Identifier;
class CircuitNodeRef;
class CircuitCurrentRef;
class BinaryOp;
class UnaryOp;
class FunctionCall;
class ArrayIndex;
class TernaryConditional;

class ExprVisitor {
 public:
  virtual ~ExprVisitor() = default;

  virtual void visit(NumberLiteral& node) = 0;
  virtual void visit(StringLiteral& node) = 0;
  virtual void visit(ArrayLiteral& node) = 0;
  virtual void visit(Identifier& node) = 0;
  virtual void visit(CircuitNodeRef& node) = 0;
  virtual void visit(CircuitCurrentRef& node) = 0;
  virtual void visit(BinaryOp& node) = 0;
  virtual void visit(UnaryOp& node) = 0;
  virtual void visit(FunctionCall& node) = 0;
  virtual void visit(ArrayIndex& node) = 0;
  virtual void visit(TernaryConditional& node) = 0;
};

class ConstExprVisitor {
 public:
  virtual ~ConstExprVisitor() = default;

  virtual void visit(const NumberLiteral& node) = 0;
  virtual void visit(const StringLiteral& node) = 0;
  virtual void visit(const ArrayLiteral& node) = 0;
  virtual void visit(const Identifier& node) = 0;
  virtual void visit(const CircuitNodeRef& node) = 0;
  virtual void visit(const CircuitCurrentRef& node) = 0;
  virtual void visit(const BinaryOp& node) = 0;
  virtual void visit(const UnaryOp& node) = 0;
  virtual void visit(const FunctionCall& node) = 0;
  virtual void visit(const ArrayIndex& node) = 0;
  virtual void visit(const TernaryConditional& node) = 0;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_VISITOR_VISITOR_H
