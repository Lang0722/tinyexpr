#ifndef SPICE_EXPR_ENGINE_EXPRESSION_ENGINE_H
#define SPICE_EXPR_ENGINE_EXPRESSION_ENGINE_H

#include <complex>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include "spice_expr/analysis/evaluation_order.h"
#include "spice_expr/ast/arena.h"
#include "spice_expr/circuit/circuit_interface.h"
#include "spice_expr/function/function_registry.h"
#include "spice_expr/symbol/symbol_table.h"
#include "spice_expr/visitor/differentiator.h"

namespace spice_expr {

class ExprNode;

class ExpressionEngine {
 public:
  ExpressionEngine();
  ~ExpressionEngine();

  ExprArena& arena() { return arena_; }
  const ExprArena& arena() const { return arena_; }

  SymbolTable& symbols() { return symbols_; }
  const SymbolTable& symbols() const { return symbols_; }

  FunctionRegistry& functions() { return functions_; }
  const FunctionRegistry& functions() const { return functions_; }

  void define_parameter(const std::string& name, ExprNode* expression);
  void define_function(const std::string& name, const std::vector<std::string>& parameters,
                       ExprNode* body);

  void set_parameter_override(const std::string& name, ExprNode* expression);
  void clear_parameter_override(const std::string& name);
  void clear_all_overrides();

  void build_evaluation_order();
  bool has_valid_evaluation_order() const;
  const std::vector<std::string>& evaluation_order() const;

  bool has_cyclic_dependencies() const;
  std::vector<std::string> get_cyclic_dependency_path() const;

  double evaluate_real(const ExprNode& expr, const CircuitInterface& circuit) const;
  std::complex<double> evaluate_complex(const ExprNode& expr,
                                        const CircuitInterface& circuit) const;

  double evaluate_real(const ExprNode& expr) const;
  std::complex<double> evaluate_complex(const ExprNode& expr) const;

  void evaluate_all_parameters(const CircuitInterface& circuit);

  ExprNode* simplify(const ExprNode& expr);
  ExprNode* simplify_to_fixpoint(const ExprNode& expr, int maxIterations = 100);

  ExprNode* differentiate(const ExprNode& expr, const DiffTarget& target);

  std::unique_ptr<SymbolTable> create_subcircuit_scope();

  void set_parameter_flag(const std::string& name, const std::string& flag);
  void clear_parameter_flag(const std::string& name, const std::string& flag);
  bool has_parameter_flag(const std::string& name, const std::string& flag) const;
  std::vector<ExprNode*> get_parameters_with_flag(const std::string& flag) const;
  std::set<ExprNode*> get_affected_parameters(const ExprNode& expr) const;
  std::set<ExprNode*> get_affected_parameters_by_flag(const std::string& flag) const;

 private:
  mutable ExprArena arena_;
  SymbolTable symbols_;
  FunctionRegistry functions_;
  EvaluationOrderManager orderManager_;
  NullCircuitInterface nullCircuit_;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_ENGINE_EXPRESSION_ENGINE_H
