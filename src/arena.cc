#include "spice_expr/ast/arena.h"

#include "spice_expr/ast/expr_node.h"

namespace spice_expr {

ExprArena::~ExprArena() {
  clear();
}

ExprArena::ExprArena(ExprArena&& other) noexcept : nodes_(std::move(other.nodes_)) {
  other.nodes_.clear();
}

ExprArena& ExprArena::operator=(ExprArena&& other) noexcept {
  if (this != &other) {
    clear();
    nodes_ = std::move(other.nodes_);
    other.nodes_.clear();
  }
  return *this;
}

void ExprArena::clear() {
  for (ExprNode* node : nodes_) {
    delete node;
  }
  nodes_.clear();
}

}  // namespace spice_expr
