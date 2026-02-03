#ifndef SPICE_EXPR_ANALYSIS_EVALUATION_ORDER_H
#define SPICE_EXPR_ANALYSIS_EVALUATION_ORDER_H

#include <memory>
#include <optional>
#include <set>
#include <string>
#include <vector>

#include "spice_expr/analysis/dependency_graph.h"

namespace spice_expr {

class SymbolTable;

class EvaluationOrderManager {
 public:
  EvaluationOrderManager() = default;

  void build_from_symbol_table(const SymbolTable& symbols);
  void invalidate();
  bool is_valid() const { return valid_; }

  const std::vector<std::string>& evaluation_order() const { return order_; }
  const DependencyGraph& dependency_graph() const { return *graph_; }

  bool has_cycle() const;
  std::optional<std::vector<std::string>> get_cycle() const;

  std::set<std::string> get_affected_parameters(std::string_view name) const;
  std::set<std::string> get_affected_parameters(const std::set<std::string>& names) const;

 private:
  bool valid_ = false;
  std::vector<std::string> order_;
  std::unique_ptr<DependencyGraph> graph_;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_ANALYSIS_EVALUATION_ORDER_H
