#include "spice_expr/analysis/dependency_graph.h"

#include <algorithm>
#include <queue>

namespace spice_expr {

void DependencyGraph::add_node(std::string_view name) {
  std::string key{name};
  nodes_.try_emplace(key, DependencyNode(std::string{name}));
}

void DependencyGraph::add_dependency(std::string_view from, std::string_view to) {
  add_node(from);
  add_node(to);

  std::string from_key{from};
  std::string to_key{to};
  nodes_.at(from_key).dependsOn.insert(to_key);
  nodes_.at(to_key).dependents.insert(from_key);
}

void DependencyGraph::remove_dependency(std::string_view from, std::string_view to) {
  std::string from_key{from};
  std::string to_key{to};
  auto fromIt = nodes_.find(from_key);
  auto toIt = nodes_.find(to_key);

  if (fromIt != nodes_.end()) {
    fromIt->second.dependsOn.erase(to_key);
  }
  if (toIt != nodes_.end()) {
    toIt->second.dependents.erase(from_key);
  }
}

const DependencyNode* DependencyGraph::get_node(std::string_view name) const {
  std::string key{name};
  auto it = nodes_.find(key);
  return (it != nodes_.end()) ? &it->second : nullptr;
}

std::set<std::string> DependencyGraph::get_dependencies(std::string_view name) const {
  std::string key{name};
  auto it = nodes_.find(key);
  if (it != nodes_.end()) {
    return it->second.dependsOn;
  }
  return {};
}

std::set<std::string> DependencyGraph::get_dependents(std::string_view name) const {
  std::string key{name};
  auto it = nodes_.find(key);
  if (it != nodes_.end()) {
    return it->second.dependents;
  }
  return {};
}

std::set<std::string> DependencyGraph::get_transitive_dependents(std::string_view name) const {
  std::set<std::string> result;
  std::queue<std::string> queue;
  std::string key{name};

  auto it = nodes_.find(key);
  if (it == nodes_.end()) {
    return result;
  }

  for (const auto& dep : it->second.dependents) {
    queue.push(dep);
  }

  while (!queue.empty()) {
    std::string current = queue.front();
    queue.pop();

    if (result.count(current) > 0) {
      continue;
    }
    result.insert(current);

    auto nodeIt = nodes_.find(current);
    if (nodeIt != nodes_.end()) {
      for (const auto& dep : nodeIt->second.dependents) {
        if (result.count(dep) == 0) {
          queue.push(dep);
        }
      }
    }
  }

  return result;
}

std::set<std::string> DependencyGraph::get_transitive_dependents(
    const std::set<std::string>& names) const {
  std::set<std::string> result;
  for (const auto& name : names) {
    auto deps = get_transitive_dependents(name);
    result.insert(deps.begin(), deps.end());
  }
  return result;
}

bool DependencyGraph::has_cycle() const {
  std::set<std::string> visited;
  std::set<std::string> recStack;
  std::vector<std::string> path;

  for (const auto& [name, node] : nodes_) {
    if (visited.find(name) == visited.end()) {
      if (detect_cycle_dfs(name, visited, recStack, path)) {
        return true;
      }
    }
  }
  return false;
}

std::optional<std::vector<std::string>> DependencyGraph::find_cycle() const {
  std::set<std::string> visited;
  std::set<std::string> recStack;
  std::vector<std::string> path;

  for (const auto& [name, node] : nodes_) {
    if (visited.find(name) == visited.end()) {
      path.clear();
      if (detect_cycle_dfs(name, visited, recStack, path)) {
        return path;
      }
    }
  }
  return std::nullopt;
}

bool DependencyGraph::detect_cycle_dfs(const std::string& node, std::set<std::string>& visited,
                                       std::set<std::string>& recStack,
                                       std::vector<std::string>& path) const {
  visited.insert(node);
  recStack.insert(node);
  path.push_back(node);

  auto it = nodes_.find(node);
  if (it != nodes_.end()) {
    for (const auto& dep : it->second.dependsOn) {
      if (recStack.find(dep) != recStack.end()) {
        path.push_back(dep);
        return true;
      }
      if (visited.find(dep) == visited.end()) {
        if (detect_cycle_dfs(dep, visited, recStack, path)) {
          return true;
        }
      }
    }
  }

  path.pop_back();
  recStack.erase(node);
  return false;
}

std::optional<std::vector<std::string>> DependencyGraph::topological_sort() const {
  // Note: Kahn's algorithm naturally detects cycles (result.size() != nodes_.size())
  // so we don't need to call has_cycle() separately

  std::unordered_map<std::string, int> inDegree;
  for (const auto& [name, node] : nodes_) {
    auto [it, inserted] = inDegree.try_emplace(name, 0);
    it->second += static_cast<int>(node.dependsOn.size());
  }

  std::queue<std::string> queue;
  for (const auto& [name, degree] : inDegree) {
    if (degree == 0) {
      queue.push(name);
    }
  }

  std::vector<std::string> result;
  while (!queue.empty()) {
    std::string current = queue.front();
    queue.pop();
    result.push_back(current);

    auto it = nodes_.find(current);
    if (it != nodes_.end()) {
      for (const auto& dependent : it->second.dependents) {
        inDegree[dependent]--;
        if (inDegree[dependent] == 0) {
          queue.push(dependent);
        }
      }
    }
  }

  if (result.size() != nodes_.size()) {
    return std::nullopt;
  }

  return result;
}

std::vector<std::string> DependencyGraph::all_nodes() const {
  std::vector<std::string> result;
  result.reserve(nodes_.size());
  for (const auto& [name, node] : nodes_) {
    result.push_back(name);
  }
  return result;
}

void DependencyGraph::clear() {
  nodes_.clear();
}

}  // namespace spice_expr
