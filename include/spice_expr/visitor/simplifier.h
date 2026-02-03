#ifndef SPICE_EXPR_VISITOR_SIMPLIFIER_H
#define SPICE_EXPR_VISITOR_SIMPLIFIER_H

#include <complex>  // for std::complex in get_complex_value return type
#include <vector>

#include "spice_expr/ast/operations.h"  // for BinaryOpType
#include "spice_expr/visitor/visitor.h"

namespace spice_expr {

class ExprArena;
class ExprNode;
class SymbolTable;

class Simplifier : public ConstExprVisitor {
 public:
  explicit Simplifier(ExprArena& arena, const SymbolTable* symbols = nullptr);

  ExprNode* simplify(const ExprNode& node);
  ExprNode* simplify_to_fixpoint(const ExprNode& node, int maxIterations = 100);

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
  static bool is_zero(const ExprNode& node);
  static bool is_one(const ExprNode& node);
  static bool is_constant(const ExprNode& node);
  static bool is_complex_constant(const ExprNode& node);
  static double get_constant_value(const ExprNode& node);
  static std::complex<double> get_complex_value(const ExprNode& node);

  // Commutativity exploitation helpers
  static bool is_commutative_associative(BinaryOpType op);
  static void collect_operands(const ExprNode& node, BinaryOpType op,
                               std::vector<ExprNode*>& operands);
  ExprNode* fold_constants(const std::vector<ExprNode*>& operands,
                           BinaryOpType op);

  ExprArena& arena_;
  const SymbolTable* symbols_;
  ExprNode* result_;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_VISITOR_SIMPLIFIER_H
