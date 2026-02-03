#ifndef SPICE_EXPR_AST_ARENA_H
#define SPICE_EXPR_AST_ARENA_H

#include <cstddef>
#include <memory>
#include <vector>

namespace spice_expr {

class ExprNode;

class ExprArena {
 public:
  ExprArena() = default;
  ~ExprArena();

  ExprArena(const ExprArena&) = delete;
  ExprArena& operator=(const ExprArena&) = delete;
  ExprArena(ExprArena&&) noexcept;
  ExprArena& operator=(ExprArena&&) noexcept;

  template <typename T, typename... Args>
  T* make(Args&&... args) {
    static_assert(std::is_base_of_v<ExprNode, T>, "T must derive from ExprNode");
    T* node = new T(std::forward<Args>(args)...);
    nodes_.push_back(node);
    return node;
  }

  void clear();
  size_t size() const { return nodes_.size(); }

 private:
  std::vector<ExprNode*> nodes_;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_AST_ARENA_H
