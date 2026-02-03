#include "spice_expr/symbol/symbol_table.h"

#include "spice_expr/ast/expr_node.h"

namespace spice_expr {

SymbolTable::SymbolTable(ScopingMode mode) : parent_(nullptr), mode_(mode) {}

SymbolTable::SymbolTable(SymbolTable* parent, ScopingMode mode) : parent_(parent), mode_(mode) {}

void SymbolTable::define(std::string_view name, ExprNode* expression) {
  std::string key{name};
  symbols_.erase(key);
  direct_values_.erase(key);  // Clear any direct value binding
  overrides_.erase(key);      // Clear any override for this symbol
  originalExpressions_.erase(key);  // Clear cached original expression
  symbols_.emplace(key, SymbolEntry(std::string{name}, expression));
}

void SymbolTable::bind_value(std::string_view name, double value) {
  direct_values_[std::string{name}] = value;
}

void SymbolTable::set_override(std::string_view name, ExprNode* expression) {
  overrides_[std::string{name}] = expression;
}

void SymbolTable::clear_override(std::string_view name) {
  overrides_.erase(std::string{name});
}

void SymbolTable::clear_all_overrides() {
  overrides_.clear();
}

const SymbolEntry* SymbolTable::lookup(std::string_view name) const {
  if (mode_ == ScopingMode::Local) {
    return lookup_local(name);
  } else {
    return lookup_global(name);
  }
}

const SymbolEntry* SymbolTable::lookup_local(std::string_view name) const {
  std::string key{name};
  auto it = symbols_.find(key);
  if (it != symbols_.end()) {
    auto overrideIt = overrides_.find(key);
    if (overrideIt != overrides_.end()) {
      // Store original expression if not already stored (originalExpressions_ is mutable)
      originalExpressions_.try_emplace(key, it->second.expression);
      const_cast<SymbolEntry&>(it->second).expression = overrideIt->second;
    } else {
      // No override - restore original if we have one stored
      auto origIt = originalExpressions_.find(key);
      if (origIt != originalExpressions_.end()) {
        const_cast<SymbolEntry&>(it->second).expression = origIt->second;
      }
    }
    return &it->second;
  }

  if (parent_) {
    return parent_->lookup_local(name);
  }
  return nullptr;
}

const SymbolEntry* SymbolTable::lookup_global(std::string_view name) const {
  std::string key{name};
  const SymbolTable* root = this;
  while (root->parent_) {
    root = root->parent_;
  }

  // In global scoping mode, root symbols take precedence
  auto rootIt = root->symbols_.find(key);
  if (rootIt != root->symbols_.end()) {
    // Check for override in current table first (child can override root symbols)
    auto overrideIt = overrides_.find(key);
    if (overrideIt != overrides_.end()) {
      root->originalExpressions_.try_emplace(key, rootIt->second.expression);
      const_cast<SymbolEntry&>(rootIt->second).expression = overrideIt->second;
      return &rootIt->second;
    }
    // Check for override in root table
    auto rootOverrideIt = root->overrides_.find(key);
    if (rootOverrideIt != root->overrides_.end()) {
      root->originalExpressions_.try_emplace(key, rootIt->second.expression);
      const_cast<SymbolEntry&>(rootIt->second).expression = rootOverrideIt->second;
      return &rootIt->second;
    }
    // No override - restore original if we have one stored
    auto origIt = root->originalExpressions_.find(key);
    if (origIt != root->originalExpressions_.end()) {
      const_cast<SymbolEntry&>(rootIt->second).expression = origIt->second;
    }
    return &rootIt->second;
  }

  // Fall back to current table's symbols (for local symbols not in root)
  auto it = symbols_.find(key);
  if (it != symbols_.end()) {
    auto overrideIt = overrides_.find(key);
    if (overrideIt != overrides_.end()) {
      originalExpressions_.try_emplace(key, it->second.expression);
      const_cast<SymbolEntry&>(it->second).expression = overrideIt->second;
    } else {
      auto origIt = originalExpressions_.find(key);
      if (origIt != originalExpressions_.end()) {
        const_cast<SymbolEntry&>(it->second).expression = origIt->second;
      }
    }
    return &it->second;
  }

  return nullptr;
}

std::optional<double> SymbolTable::lookup_direct_value(std::string_view name) const {
  std::string key{name};
  auto it = direct_values_.find(key);
  if (it != direct_values_.end()) {
    return it->second;
  }
  if (parent_) {
    return parent_->lookup_direct_value(name);
  }
  return std::nullopt;
}

bool SymbolTable::exists(std::string_view name) const {
  return lookup(name) != nullptr || lookup_direct_value(name).has_value();
}

std::unique_ptr<SymbolTable> SymbolTable::create_child() const {
  return std::make_unique<SymbolTable>(const_cast<SymbolTable*>(this), mode_);
}

std::vector<std::string> SymbolTable::all_symbol_names() const {
  std::vector<std::string> names;
  names.reserve(symbols_.size());
  for (const auto& [name, entry] : symbols_) {
    names.push_back(name);
  }
  return names;
}

void SymbolTable::set_flag(std::string_view name, std::string_view flag) {
  std::string key{name};
  auto it = symbols_.find(key);
  if (it != symbols_.end()) {
    it->second.flags.insert(std::string{flag});
  }
}

void SymbolTable::clear_flag(std::string_view name, std::string_view flag) {
  std::string key{name};
  auto it = symbols_.find(key);
  if (it != symbols_.end()) {
    it->second.flags.erase(std::string{flag});
  }
}

void SymbolTable::clear_all_flags(std::string_view name) {
  std::string key{name};
  auto it = symbols_.find(key);
  if (it != symbols_.end()) {
    it->second.flags.clear();
  }
}

bool SymbolTable::has_flag(std::string_view name, std::string_view flag) const {
  std::string key{name};
  auto it = symbols_.find(key);
  if (it != symbols_.end()) {
    return it->second.flags.count(std::string{flag}) > 0;
  }
  return false;
}

std::set<std::string> SymbolTable::get_flags(std::string_view name) const {
  std::string key{name};
  auto it = symbols_.find(key);
  if (it != symbols_.end()) {
    return it->second.flags;
  }
  return {};
}

std::vector<ExprNode*> SymbolTable::get_parameters_with_flag(std::string_view flag) const {
  std::vector<ExprNode*> result;
  std::string flagKey{flag};
  for (const auto& [name, entry] : symbols_) {
    if (entry.flags.count(flagKey) > 0) {
      result.push_back(entry.expression);
    }
  }
  return result;
}

}  // namespace spice_expr
