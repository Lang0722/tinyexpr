#ifndef SPICE_EXPR_ANALYSIS_DEPENDENCY_PRINTER_H
#define SPICE_EXPR_ANALYSIS_DEPENDENCY_PRINTER_H

#include <ostream>
#include <string>
#include <string_view>
#include <vector>

namespace spice_expr {

class ExprNode;
class SymbolTable;

// Represents a scope in the hierarchy for traversal
struct ScopeHierarchy {
  const SymbolTable* scope;
  std::string name;
  std::vector<ScopeHierarchy> children;
};

class DependencyPrinter {
 public:
  DependencyPrinter() = default;

  // Main API - print hierarchy with explicit structure
  std::string print(const ScopeHierarchy& hierarchy) const;
  void print(const ScopeHierarchy& hierarchy, std::ostream& os) const;

  // Single scope (no hierarchy)
  std::string print(const SymbolTable& symbols, std::string_view scope_name = "root") const;

  // Configuration
  void set_show_expressions(bool show) { show_expressions_ = show; }
  bool show_expressions() const { return show_expressions_; }

 private:
  struct ScopedParameter {
    std::string name;
    std::string scope_path;
    const ExprNode* expression;
  };

  struct ScopedDependency {
    std::string from_param;
    std::string from_scope;
    std::string to_param;
    std::string to_scope;
    bool is_cross_scope;
  };

  // Collect parameters and dependencies from a scope hierarchy
  static void collect_from_hierarchy(const ScopeHierarchy& hierarchy, const std::string& parent_path,
                                     std::vector<ScopedParameter>& params,
                                     std::vector<ScopedDependency>& deps);

  // Collect from a single scope
  static void collect_from_scope(const SymbolTable& scope, const std::string& scope_path,
                                 std::vector<ScopedParameter>& params,
                                 std::vector<ScopedDependency>& deps);

  // Find which scope defines a parameter by walking up the parent chain
  static std::string find_defining_scope(const SymbolTable& from_scope, std::string_view param_name,
                                         const std::string& current_scope_path);

  // Format collected data as text output
  std::string format_output(const std::vector<ScopedParameter>& params,
                            const std::vector<ScopedDependency>& deps) const;

  bool show_expressions_ = true;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_ANALYSIS_DEPENDENCY_PRINTER_H
