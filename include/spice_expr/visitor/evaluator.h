#ifndef SPICE_EXPR_VISITOR_EVALUATOR_H
#define SPICE_EXPR_VISITOR_EVALUATOR_H

#include <complex>
#include <iostream>
#include <stack>
#include <stdexcept>
#include <string>
#include <unordered_set>
#include <variant>

#include "spice_expr/circuit/circuit_interface.h"
#include "spice_expr/visitor/visitor.h"

namespace spice_expr {

class SymbolTable;
class FunctionRegistry;
class ExprNode;
class ExprArena;
class XTensor;

class EvaluationError : public std::runtime_error {
 public:
  explicit EvaluationError(const std::string& msg) : std::runtime_error(msg) {}
};

class RealEvaluator : public ConstExprVisitor {
 public:
  RealEvaluator(const SymbolTable& symbols, const FunctionRegistry& functions,
                const CircuitInterface& circuit, ExprArena* arena = nullptr);

  double evaluate(const ExprNode& node);
  double result() const;

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
  double pop();

  const SymbolTable& symbols_;
  const FunctionRegistry& functions_;
  const CircuitInterface& circuit_;
  ExprArena* arena_;
  std::stack<double> stack_;
};

class ComplexEvaluator : public ConstExprVisitor {
 public:
  ComplexEvaluator(const SymbolTable& symbols, const FunctionRegistry& functions,
                   const CircuitInterface& circuit, ExprArena* arena = nullptr);

  std::complex<double> evaluate(const ExprNode& node);
  std::complex<double> result() const;

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
  std::complex<double> pop();
  static void warn_complex_fallback(const char* op_name);

  const SymbolTable& symbols_;
  const FunctionRegistry& functions_;
  const CircuitInterface& circuit_;
  ExprArena* arena_;
  std::stack<std::complex<double>> stack_;

  // One-time warning tracking for complex fallback operations
  static inline std::unordered_set<std::string> warned_ops_;
};

bool requires_complex_evaluation(const ExprNode& node);

// Unified evaluation result type for array-aware evaluation
using EvalValue = std::variant<double, std::complex<double>, XTensor>;

// Helper functions for EvalValue operations
struct EvalValueOps {
  static bool is_scalar(const EvalValue& v);
  static bool is_array(const EvalValue& v);
  static bool is_real(const EvalValue& v);
  static bool is_complex(const EvalValue& v);

  static double to_real(const EvalValue& v);
  static std::complex<double> to_complex(const EvalValue& v);
  static XTensor to_array(const EvalValue& v);
};

// Detection function for array evaluation
bool requires_array_evaluation(const ExprNode& node, const SymbolTable& symbols);

}  // namespace spice_expr

#endif  // SPICE_EXPR_VISITOR_EVALUATOR_H
