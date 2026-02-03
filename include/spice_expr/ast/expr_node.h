#ifndef SPICE_EXPR_AST_EXPR_NODE_H
#define SPICE_EXPR_AST_EXPR_NODE_H

#include <cstddef>
#include <cstdint>
#include <string>

namespace spice_expr {

class ExprArena;
class ExprVisitor;
class ConstExprVisitor;

enum class NodeType : uint8_t {
  NumberLiteral,
  StringLiteral,
  ArrayLiteral,
  Identifier,
  CircuitNodeRef,
  CircuitCurrentRef,
  BinaryOp,
  UnaryOp,
  FunctionCall,
  ArrayIndex,
  TernaryConditional
};

struct SourceLocation {
  int line = 0;
  int column = 0;
  std::string filename;

  SourceLocation() = default;
  SourceLocation(int l, int c, std::string f = "") : line(l), column(c), filename(std::move(f)) {}
};

class ExprNode {
 public:
  explicit ExprNode(NodeType type) : type_(type) {}
  virtual ~ExprNode() = default;

  ExprNode(const ExprNode&) = delete;
  ExprNode& operator=(const ExprNode&) = delete;

  NodeType type() const { return type_; }

  const SourceLocation& location() const { return location_; }
  void set_location(const SourceLocation& loc) { location_ = loc; }

  virtual void accept(ExprVisitor& visitor) = 0;
  virtual void accept(ConstExprVisitor& visitor) const = 0;
  virtual ExprNode* clone(ExprArena& arena) const = 0;
  virtual size_t hash() const = 0;
  virtual bool equals(const ExprNode& other) const = 0;

 protected:
  static size_t combineHash(size_t h1, size_t h2) {
    return h1 ^ (h2 + 0x9e3779b9 + (h1 << 6) + (h1 >> 2));
  }

 private:
  NodeType type_;
  SourceLocation location_;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_AST_EXPR_NODE_H
