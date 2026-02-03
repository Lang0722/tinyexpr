#ifndef SPICE_EXPR_ANALYSIS_DEPENDENCY_GRAPH_H
#define SPICE_EXPR_ANALYSIS_DEPENDENCY_GRAPH_H

#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace spice_expr {

struct DependencyNode {
  std::string name;
  std::set<std::string> dependsOn;
  std::set<std::string> dependents;

  explicit DependencyNode(std::string n) : name(std::move(n)) {}
};

class DependencyGraph {
 public:
  DependencyGraph() = default;

  void add_node(std::string_view name);
  void add_dependency(std::string_view from, std::string_view to);
  void remove_dependency(std::string_view from, std::string_view to);

  const DependencyNode* get_node(std::string_view name) const;
  std::set<std::string> get_dependencies(std::string_view name) const;
  std::set<std::string> get_dependents(std::string_view name) const;

  std::set<std::string> get_transitive_dependents(std::string_view name) const;
  std::set<std::string> get_transitive_dependents(const std::set<std::string>& names) const;

  bool has_cycle() const;
  std::optional<std::vector<std::string>> find_cycle() const;

  std::optional<std::vector<std::string>> topological_sort() const;

  std::vector<std::string> all_nodes() const;
  void clear();

 private:
  bool detect_cycle_dfs(const std::string& node, std::set<std::string>& visited,
                        std::set<std::string>& recStack, std::vector<std::string>& path) const;

  std::unordered_map<std::string, DependencyNode> nodes_;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_ANALYSIS_DEPENDENCY_GRAPH_H
