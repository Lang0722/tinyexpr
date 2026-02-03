#include "spice_expr/analysis/dependency_printer.h"

#include <algorithm>
#include <map>
#include <sstream>

#include "spice_expr/symbol/symbol_table.h"
#include "spice_expr/visitor/dependency_extractor.h"
#include "spice_expr/visitor/printer.h"

namespace spice_expr {

std::string DependencyPrinter::print(const ScopeHierarchy& hierarchy) const {
  std::vector<ScopedParameter> params;
  std::vector<ScopedDependency> deps;
  collect_from_hierarchy(hierarchy, "", params, deps);
  return format_output(params, deps);
}

void DependencyPrinter::print(const ScopeHierarchy& hierarchy, std::ostream& os) const {
  os << print(hierarchy);
}

std::string DependencyPrinter::print(const SymbolTable& symbols, std::string_view scope_name) const {
  std::vector<ScopedParameter> params;
  std::vector<ScopedDependency> deps;
  collect_from_scope(symbols, std::string{scope_name}, params, deps);
  return format_output(params, deps);
}

void DependencyPrinter::collect_from_hierarchy(const ScopeHierarchy& hierarchy,
                                               const std::string& parent_path,
                                               std::vector<ScopedParameter>& params,
                                               std::vector<ScopedDependency>& deps) const {
  std::string scope_path = parent_path.empty() ? hierarchy.name : parent_path + "/" + hierarchy.name;

  collect_from_scope(*hierarchy.scope, scope_path, params, deps);

  for (const auto& child : hierarchy.children) {
    collect_from_hierarchy(child, scope_path, params, deps);
  }
}

void DependencyPrinter::collect_from_scope(const SymbolTable& scope, const std::string& scope_path,
                                           std::vector<ScopedParameter>& params,
                                           std::vector<ScopedDependency>& deps) const {
  DependencyExtractor extractor;

  for (const auto& name : scope.all_symbol_names()) {
    const SymbolEntry* entry = scope.lookup(name);
    if (!entry || !entry->expression) {
      continue;
    }

    ScopedParameter param;
    param.name = name;
    param.scope_path = scope_path;
    param.expression = entry->expression;
    params.push_back(param);

    auto expr_deps = extractor.extract(*entry->expression);
    for (const auto& dep_name : expr_deps.parameters) {
      std::string defining_scope = find_defining_scope(scope, dep_name, scope_path);

      ScopedDependency dep;
      dep.from_param = name;
      dep.from_scope = scope_path;
      dep.to_param = dep_name;
      dep.to_scope = defining_scope;
      dep.is_cross_scope = (scope_path != defining_scope);
      deps.push_back(dep);
    }
  }
}

std::string DependencyPrinter::find_defining_scope(const SymbolTable& from_scope,
                                                   std::string_view param_name,
                                                   const std::string& current_scope_path) const {
  // For Local scoping mode: walk up the parent chain to find who owns this symbol
  // For Global scoping mode: the lookup will find the symbol in root or current scope

  // Check if the symbol exists in the current scope's local symbols
  const auto& local_names = from_scope.all_symbol_names();
  if (std::find(local_names.begin(), local_names.end(), std::string{param_name}) !=
      local_names.end()) {
    return current_scope_path;
  }

  // Walk up the parent chain
  const SymbolTable* parent = from_scope.parent();
  if (parent) {
    // We need to compute the parent scope path by removing the last component
    auto last_slash = current_scope_path.rfind('/');
    std::string parent_path;
    if (last_slash != std::string::npos) {
      parent_path = current_scope_path.substr(0, last_slash);
    } else {
      // We're at root level with no parent path component
      parent_path = current_scope_path;  // This shouldn't normally happen if paths are correct
    }
    return find_defining_scope(*parent, param_name, parent_path);
  }

  // Not found anywhere - return current scope as best guess (undefined parameter)
  return current_scope_path;
}

std::string DependencyPrinter::format_output(const std::vector<ScopedParameter>& params,
                                             const std::vector<ScopedDependency>& deps) const {
  std::ostringstream ss;
  ss << "=== Dependency Relations ===\n";

  // Group parameters by scope
  std::map<std::string, std::vector<const ScopedParameter*>> params_by_scope;
  for (const auto& param : params) {
    params_by_scope[param.scope_path].push_back(&param);
  }

  // Build a lookup for dependencies by (from_scope, from_param)
  std::map<std::pair<std::string, std::string>, std::vector<const ScopedDependency*>> deps_by_param;
  for (const auto& dep : deps) {
    deps_by_param[{dep.from_scope, dep.from_param}].push_back(&dep);
  }

  // Collect cross-scope dependencies for summary
  std::vector<const ScopedDependency*> cross_scope_deps;
  for (const auto& dep : deps) {
    if (dep.is_cross_scope) {
      cross_scope_deps.push_back(&dep);
    }
  }

  ExprPrinter expr_printer(PrintFormat::SPICE);

  // Print parameters grouped by scope
  for (const auto& [scope_path, scope_params] : params_by_scope) {
    ss << "\nScope: " << scope_path << "\n";

    for (const auto* param : scope_params) {
      ss << "  " << param->name;
      if (show_expressions_ && param->expression) {
        ss << " = " << expr_printer.print(*param->expression);
      }
      ss << "\n";

      auto it = deps_by_param.find({param->scope_path, param->name});
      if (it != deps_by_param.end() && !it->second.empty()) {
        for (const auto* dep : it->second) {
          ss << "    depends on: " << dep->to_param << " [" << dep->to_scope << "]";
          if (dep->is_cross_scope) {
            ss << " (cross-scope)";
          }
          ss << "\n";
        }
      } else {
        ss << "    (no dependencies)\n";
      }
    }
  }

  // Print cross-scope summary if any
  if (!cross_scope_deps.empty()) {
    ss << "\nCross-scope Summary:\n";
    for (const auto* dep : cross_scope_deps) {
      ss << "  " << dep->from_scope << "::" << dep->from_param << " -> " << dep->to_scope
         << "::" << dep->to_param << "\n";
    }
  }

  return ss.str();
}

}  // namespace spice_expr
