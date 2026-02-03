#include "spice_expr/ast/references.h"

#include <functional>

#include "spice_expr/ast/arena.h"
#include "spice_expr/visitor/visitor.h"

namespace spice_expr {

// Identifier

void Identifier::accept(ExprVisitor& visitor) {
  visitor.visit(*this);
}

void Identifier::accept(ConstExprVisitor& visitor) const {
  visitor.visit(*this);
}

ExprNode* Identifier::clone(ExprArena& arena) const {
  return arena.make<Identifier>(name_);
}

size_t Identifier::hash() const {
  return std::hash<std::string>{}(name_);
}

bool Identifier::equals(const ExprNode& other) const {
  if (other.type() != NodeType::Identifier)
    return false;
  return name_ == static_cast<const Identifier&>(other).name_;
}

// CircuitNodeRef

void CircuitNodeRef::accept(ExprVisitor& visitor) {
  visitor.visit(*this);
}

void CircuitNodeRef::accept(ConstExprVisitor& visitor) const {
  visitor.visit(*this);
}

ExprNode* CircuitNodeRef::clone(ExprArena& arena) const {
  if (differential_) {
    return arena.make<CircuitNodeRef>(node1_, node2_);
  }
  return arena.make<CircuitNodeRef>(node1_);
}

size_t CircuitNodeRef::hash() const {
  size_t h = std::hash<std::string>{}(node1_);
  if (differential_) {
    h = combineHash(h, std::hash<std::string>{}(node2_));
  }
  return h;
}

bool CircuitNodeRef::equals(const ExprNode& other) const {
  if (other.type() != NodeType::CircuitNodeRef)
    return false;
  const auto& o = static_cast<const CircuitNodeRef&>(other);
  return node1_ == o.node1_ && node2_ == o.node2_ && differential_ == o.differential_;
}

// CircuitCurrentRef

void CircuitCurrentRef::accept(ExprVisitor& visitor) {
  visitor.visit(*this);
}

void CircuitCurrentRef::accept(ConstExprVisitor& visitor) const {
  visitor.visit(*this);
}

ExprNode* CircuitCurrentRef::clone(ExprArena& arena) const {
  return arena.make<CircuitCurrentRef>(device_);
}

size_t CircuitCurrentRef::hash() const {
  return std::hash<std::string>{}(device_);
}

bool CircuitCurrentRef::equals(const ExprNode& other) const {
  if (other.type() != NodeType::CircuitCurrentRef)
    return false;
  return device_ == static_cast<const CircuitCurrentRef&>(other).device_;
}

}  // namespace spice_expr
