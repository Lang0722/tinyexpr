#include "spice_expr/analysis/evaluation_order.h"

#include "spice_expr/analysis/dependency_graph.h"
#include "spice_expr/symbol/symbol_table.h"
#include "spice_expr/visitor/dependency_extractor.h"

namespace spice_expr {

void EvaluationOrderManager::build_from_symbol_table(const SymbolTable& symbols) {
  graph_ = std::make_unique<DependencyGraph>();
  order_.clear();
  valid_ = false;

  auto names = symbols.all_symbol_names();
  for (const auto& name : names) {
    graph_->add_node(name);
  }

  DependencyExtractor extractor;
  for (const auto& name : names) {
    const SymbolEntry* entry = symbols.lookup(name);
    if (entry && entry->expression) {
      auto deps = extractor.extract(*entry->expression);
      for (const auto& dep : deps.parameters) {
        if (symbols.exists(dep)) {
          graph_->add_dependency(name, dep);
        }
      }
    }
  }

  auto sorted = graph_->topological_sort();
  if (sorted) {
    order_ = std::move(*sorted);
    valid_ = true;
  }
}

void EvaluationOrderManager::invalidate() {
  valid_ = false;
  order_.clear();
}

bool EvaluationOrderManager::has_cycle() const {
  return graph_ && graph_->has_cycle();
}

std::optional<std::vector<std::string>> EvaluationOrderManager::get_cycle() const {
  if (graph_) {
    return graph_->find_cycle();
  }
  return std::nullopt;
}

std::set<std::string> EvaluationOrderManager::get_affected_parameters(std::string_view name) const {
  if (graph_) {
    return graph_->get_transitive_dependents(name);
  }
  return {};
}

std::set<std::string> EvaluationOrderManager::get_affected_parameters(
    const std::set<std::string>& names) const {
  if (graph_) {
    return graph_->get_transitive_dependents(names);
  }
  return {};
}

}  // namespace spice_expr
