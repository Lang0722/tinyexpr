#include "spice_expr/engine/expression_engine.h"

#include "spice_expr/visitor/dependency_extractor.h"
#include "spice_expr/visitor/differentiator.h"
#include "spice_expr/visitor/evaluator.h"
#include "spice_expr/visitor/simplifier.h"

namespace spice_expr {

ExpressionEngine::ExpressionEngine() = default;

ExpressionEngine::~ExpressionEngine() = default;

void ExpressionEngine::define_parameter(const std::string& name, ExprNode* expression) {
  symbols_.define(name, expression);
  orderManager_.invalidate();
}

void ExpressionEngine::define_function(const std::string& name,
                                       const std::vector<std::string>& parameters, ExprNode* body) {
  functions_.register_user_function(std::make_unique<UserFunction>(name, parameters, body));
}

void ExpressionEngine::set_parameter_override(const std::string& name, ExprNode* expression) {
  symbols_.set_override(name, expression);
  orderManager_.invalidate();
}

void ExpressionEngine::clear_parameter_override(const std::string& name) {
  symbols_.clear_override(name);
  orderManager_.invalidate();
}

void ExpressionEngine::clear_all_overrides() {
  symbols_.clear_all_overrides();
  orderManager_.invalidate();
}

void ExpressionEngine::build_evaluation_order() {
  orderManager_.build_from_symbol_table(symbols_);
}

bool ExpressionEngine::has_valid_evaluation_order() const {
  return orderManager_.is_valid();
}

const std::vector<std::string>& ExpressionEngine::evaluation_order() const {
  return orderManager_.evaluation_order();
}

bool ExpressionEngine::has_cyclic_dependencies() const {
  return orderManager_.has_cycle();
}

std::vector<std::string> ExpressionEngine::get_cyclic_dependency_path() const {
  auto cycle = orderManager_.get_cycle();
  return cycle.value_or(std::vector<std::string>{});
}

double ExpressionEngine::evaluate_real(const ExprNode& expr,
                                       const CircuitInterface& circuit) const {
  RealEvaluator evaluator(symbols_, functions_, circuit, &arena_);
  return evaluator.evaluate(expr);
}

std::complex<double> ExpressionEngine::evaluate_complex(const ExprNode& expr,
                                                        const CircuitInterface& circuit) const {
  ComplexEvaluator evaluator(symbols_, functions_, circuit, &arena_);
  return evaluator.evaluate(expr);
}

double ExpressionEngine::evaluate_real(const ExprNode& expr) const {
  return evaluate_real(expr, nullCircuit_);
}

std::complex<double> ExpressionEngine::evaluate_complex(const ExprNode& expr) const {
  return evaluate_complex(expr, nullCircuit_);
}

void ExpressionEngine::evaluate_all_parameters(const CircuitInterface& circuit) {
  if (!has_valid_evaluation_order()) {
    build_evaluation_order();
  }

  for (const std::string& name : evaluation_order()) {
    const SymbolEntry* entry = symbols_.lookup(name);
    if (entry && entry->expression) {
      RealEvaluator evaluator(symbols_, functions_, circuit, &arena_);
      evaluator.evaluate(*entry->expression);
    }
  }
}

ExprNode* ExpressionEngine::simplify(const ExprNode& expr) {
  Simplifier simplifier(arena_, &symbols_);
  return simplifier.simplify(expr);
}

ExprNode* ExpressionEngine::simplify_to_fixpoint(const ExprNode& expr, int maxIterations) {
  Simplifier simplifier(arena_, &symbols_);
  return simplifier.simplify_to_fixpoint(expr, maxIterations);
}

ExprNode* ExpressionEngine::differentiate(const ExprNode& expr, const DiffTarget& target) {
  Differentiator diff(arena_, target, &symbols_);
  const ExprNode* diff_result = diff.differentiate(expr);
  Simplifier simplifier(arena_, &symbols_);
  return simplifier.simplify_to_fixpoint(*diff_result);
}

std::unique_ptr<SymbolTable> ExpressionEngine::create_subcircuit_scope() {
  return symbols_.create_child();
}

void ExpressionEngine::set_parameter_flag(const std::string& name, const std::string& flag) {
  symbols_.set_flag(name, flag);
}

void ExpressionEngine::clear_parameter_flag(const std::string& name, const std::string& flag) {
  symbols_.clear_flag(name, flag);
}

bool ExpressionEngine::has_parameter_flag(const std::string& name, const std::string& flag) const {
  return symbols_.has_flag(name, flag);
}

std::vector<ExprNode*> ExpressionEngine::get_parameters_with_flag(const std::string& flag) const {
  return symbols_.get_parameters_with_flag(flag);
}

std::set<ExprNode*> ExpressionEngine::get_affected_parameters(const ExprNode& expr) const {
  // Extract parameter dependencies from the expression
  DependencyExtractor extractor;
  auto deps = extractor.extract(expr);

  // Find all parameters that transitively depend on the extracted parameters
  std::set<std::string> affectedNames = orderManager_.get_affected_parameters(deps.parameters);

  // Convert names to ExprNode pointers
  std::set<ExprNode*> result;
  for (const auto& name : affectedNames) {
    const SymbolEntry* entry = symbols_.lookup(name);
    if (entry && entry->expression) {
      result.insert(entry->expression);
    }
  }
  return result;
}

std::set<ExprNode*> ExpressionEngine::get_affected_parameters_by_flag(
    const std::string& flag) const {
  // Get flagged expression nodes
  auto flaggedExprs = symbols_.get_parameters_with_flag(flag);

  // Collect parameter names for flagged expressions
  std::set<std::string> flaggedNames;
  for (const auto& name : symbols_.all_symbol_names()) {
    if (symbols_.has_flag(name, flag)) {
      flaggedNames.insert(name);
    }
  }

  // Get all transitive dependents
  auto dependentNames = orderManager_.get_affected_parameters(flaggedNames);

  // Build result set with flagged expressions and their dependents
  std::set<ExprNode*> result(flaggedExprs.begin(), flaggedExprs.end());
  for (const auto& name : dependentNames) {
    const SymbolEntry* entry = symbols_.lookup(name);
    if (entry && entry->expression) {
      result.insert(entry->expression);
    }
  }
  return result;
}

}  // namespace spice_expr
