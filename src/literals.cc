#include "spice_expr/ast/literals.h"

#include <functional>

#include "spice_expr/ast/arena.h"
#include "spice_expr/visitor/visitor.h"

namespace spice_expr {

// NumberLiteral

void NumberLiteral::accept(ExprVisitor& visitor) {
  visitor.visit(*this);
}

void NumberLiteral::accept(ConstExprVisitor& visitor) const {
  visitor.visit(*this);
}

ExprNode* NumberLiteral::clone(ExprArena& arena) const {
  if (is_complex_) {
    return arena.make<NumberLiteral>(real_, imag_);
  }
  return arena.make<NumberLiteral>(real_);
}

size_t NumberLiteral::hash() const {
  size_t h = std::hash<double>{}(real_);
  if (is_complex_) {
    h = combineHash(h, std::hash<double>{}(imag_));
  }
  return h;
}

bool NumberLiteral::equals(const ExprNode& other) const {
  if (other.type() != NodeType::NumberLiteral)
    return false;
  const auto& o = static_cast<const NumberLiteral&>(other);
  return real_ == o.real_ && imag_ == o.imag_ && is_complex_ == o.is_complex_;
}

// StringLiteral

void StringLiteral::accept(ExprVisitor& visitor) {
  visitor.visit(*this);
}

void StringLiteral::accept(ConstExprVisitor& visitor) const {
  visitor.visit(*this);
}

ExprNode* StringLiteral::clone(ExprArena& arena) const {
  return arena.make<StringLiteral>(value_);
}

size_t StringLiteral::hash() const {
  return std::hash<std::string>{}(value_);
}

bool StringLiteral::equals(const ExprNode& other) const {
  if (other.type() != NodeType::StringLiteral)
    return false;
  return value_ == static_cast<const StringLiteral&>(other).value_;
}

// ArrayLiteral

void ArrayLiteral::accept(ExprVisitor& visitor) {
  visitor.visit(*this);
}

void ArrayLiteral::accept(ConstExprVisitor& visitor) const {
  visitor.visit(*this);
}

ExprNode* ArrayLiteral::clone(ExprArena& arena) const {
  std::vector<ExprNode*> clonedElements;
  clonedElements.reserve(elements_.size());
  for (const auto* elem : elements_) {
    clonedElements.push_back(elem->clone(arena));
  }
  return arena.make<ArrayLiteral>(std::move(clonedElements));
}

size_t ArrayLiteral::hash() const {
  size_t h = 0x12345678;
  for (const ExprNode* elem : elements_) {
    h = combineHash(h, elem->hash());
  }
  return h;
}

bool ArrayLiteral::equals(const ExprNode& other) const {
  if (other.type() != NodeType::ArrayLiteral)
    return false;
  const auto& o = static_cast<const ArrayLiteral&>(other);
  if (elements_.size() != o.elements_.size())
    return false;
  for (size_t i = 0; i < elements_.size(); ++i) {
    if (!elements_[i]->equals(*o.elements_[i]))
      return false;
  }
  return true;
}

}  // namespace spice_expr
