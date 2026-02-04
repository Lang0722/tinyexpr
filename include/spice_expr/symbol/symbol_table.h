#ifndef SPICE_EXPR_SYMBOL_SYMBOL_TABLE_H
#define SPICE_EXPR_SYMBOL_SYMBOL_TABLE_H

#include <complex>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "spice_expr/array/xtensor.h"

namespace spice_expr {

class ExprNode;

enum class ScopingMode { Local, Global };

struct SymbolEntry {
  std::string name;
  ExprNode* expression;
  std::set<std::string> flags;

  SymbolEntry(std::string n, ExprNode* expr) : name(std::move(n)), expression(expr) {}
};

class SymbolTable {
 public:
  explicit SymbolTable(ScopingMode mode = ScopingMode::Local);
  explicit SymbolTable(SymbolTable* parent, ScopingMode mode = ScopingMode::Local);

  void define(std::string_view name, ExprNode* expression);
  void bind_value(std::string_view name, double value);
  void set_override(std::string_view name, ExprNode* expression);
  void clear_override(std::string_view name);
  void clear_all_overrides();

  const SymbolEntry* lookup(std::string_view name) const;
  std::optional<double> lookup_direct_value(std::string_view name) const;
  bool exists(std::string_view name) const;

  std::unique_ptr<SymbolTable> create_child() const;

  ScopingMode scoping_mode() const { return mode_; }
  void set_scoping_mode(ScopingMode mode) { mode_ = mode; }

  const SymbolTable* parent() const { return parent_; }

  std::vector<std::string> all_symbol_names() const;

  void set_flag(std::string_view name, std::string_view flag);
  void clear_flag(std::string_view name, std::string_view flag);
  void clear_all_flags(std::string_view name);
  bool has_flag(std::string_view name, std::string_view flag) const;
  std::set<std::string> get_flags(std::string_view name) const;
  std::vector<ExprNode*> get_parameters_with_flag(std::string_view flag) const;

  // Array parameter support for post-processing calculations
  void define_array(const std::string& name, const std::vector<double>& data);
  void define_array(const std::string& name, const std::vector<std::complex<double>>& data);
  bool is_array(std::string_view name) const;
  const XTensor* lookup_array(std::string_view name) const;
  void clear_array(std::string_view name);
  void clear_all_arrays();
  std::vector<std::string> all_array_names() const;

 private:
  const SymbolEntry* lookup_local(std::string_view name) const;
  const SymbolEntry* lookup_global(std::string_view name) const;

  SymbolTable* parent_;
  ScopingMode mode_;
  std::unordered_map<std::string, SymbolEntry> symbols_;
  std::unordered_map<std::string, ExprNode*> overrides_;
  mutable std::unordered_map<std::string, ExprNode*> originalExpressions_;
  std::unordered_map<std::string, double> direct_values_;  // Direct value storage for function parameters (no arena needed)
  std::unordered_map<std::string, std::unique_ptr<XTensor>> arrays_;  // Array storage for post-processing
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_SYMBOL_SYMBOL_TABLE_H
