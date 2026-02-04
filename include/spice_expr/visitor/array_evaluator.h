#ifndef SPICE_EXPR_VISITOR_ARRAY_EVALUATOR_H
#define SPICE_EXPR_VISITOR_ARRAY_EVALUATOR_H

#include <stack>

#include "spice_expr/array/xtensor.h"
#include "spice_expr/ast/operations.h"  // for BinaryOpType, UnaryOpType
#include "spice_expr/visitor/evaluator.h"
#include "spice_expr/visitor/visitor.h"

namespace spice_expr {

class SymbolTable;
class FunctionRegistry;
class ExprNode;
class ExprArena;

// Array-aware evaluator that handles both scalar and array expressions.
// Uses xtensor for N-dimensional array operations with automatic broadcasting.
class ArrayEvaluator : public ConstExprVisitor {
 public:
  ArrayEvaluator(const SymbolTable& symbols, const FunctionRegistry& functions,
                 const CircuitInterface& circuit, ExprArena* arena = nullptr);

  // Evaluate an expression and return unified result
  EvalValue evaluate(const ExprNode& node);

  // Get the current result
  EvalValue result() const { return stack_.empty() ? EvalValue{0.0} : stack_.top(); }

  // Visitor methods for all node types
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
  EvalValue pop();

  // Apply binary operation with automatic broadcasting
  EvalValue apply_binary_op(BinaryOpType op, const EvalValue& lhs, const EvalValue& rhs);

  // Apply unary operation
  EvalValue apply_unary_op(UnaryOpType op, const EvalValue& operand);

  const SymbolTable& symbols_;
  const FunctionRegistry& functions_;
  const CircuitInterface& circuit_;
  ExprArena* arena_;
  std::stack<EvalValue> stack_;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_VISITOR_ARRAY_EVALUATOR_H
