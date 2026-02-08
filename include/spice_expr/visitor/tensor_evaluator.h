#ifndef SPICE_EXPR_VISITOR_TENSOR_EVALUATOR_H
#define SPICE_EXPR_VISITOR_TENSOR_EVALUATOR_H

#include <stack>

#include "spice_expr/array/xtensor.h"
#include "spice_expr/ast/operations.h"
#include "spice_expr/circuit/circuit_interface.h"
#include "spice_expr/visitor/visitor.h"

namespace spice_expr {

class SymbolTable;
class FunctionRegistry;
class ExprNode;
class ExprArena;

// Detect whether an expression requires tensor (array) evaluation.
bool requires_tensor_evaluation(const ExprNode& node, const SymbolTable& symbols);

// Tensor-aware evaluator that handles both scalar and array expressions.
// Uses tinytensor for N-dimensional array operations with automatic broadcasting.
// Scalars are represented as shape-{1} tensors for uniform handling.
class TensorEvaluator : public ConstExprVisitor {
 public:
  TensorEvaluator(const SymbolTable& symbols, const FunctionRegistry& functions,
                  const CircuitInterface& circuit, ExprArena* arena = nullptr);

  XTensor evaluate(const ExprNode& node);

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
  XTensor pop();

  // Real tensor operations
  static XTensor apply_real_binary(BinaryOpType op, const RealXTensor& lhs,
                                   const RealXTensor& rhs);
  static XTensor apply_real_unary(UnaryOpType op, const RealXTensor& operand);
  static XTensor apply_real_func(const std::string& name, const RealXTensor& arg);

  // Complex tensor operations
  static XTensor apply_complex_binary(BinaryOpType op, const ComplexXTensor& lhs,
                                      const ComplexXTensor& rhs);
  static XTensor apply_complex_unary(UnaryOpType op, const ComplexXTensor& operand);
  static XTensor apply_complex_func(const std::string& name, const ComplexXTensor& arg);

  const SymbolTable& symbols_;
  const FunctionRegistry& functions_;
  const CircuitInterface& circuit_;
  ExprArena* arena_;
  std::stack<XTensor> stack_;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_VISITOR_TENSOR_EVALUATOR_H
