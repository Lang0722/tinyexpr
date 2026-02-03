#include "spice_expr/ast/functions.h"

#include <functional>

#include "spice_expr/ast/arena.h"
#include "spice_expr/visitor/visitor.h"

namespace spice_expr {

// FunctionCall

void FunctionCall::accept(ExprVisitor& visitor) {
  visitor.visit(*this);
}

void FunctionCall::accept(ConstExprVisitor& visitor) const {
  visitor.visit(*this);
}

ExprNode* FunctionCall::clone(ExprArena& arena) const {
  std::vector<ExprNode*> clonedArgs;
  clonedArgs.reserve(arguments_.size());
  for (const auto* arg : arguments_) {
    clonedArgs.push_back(arg->clone(arena));
  }
  return arena.make<FunctionCall>(name_, std::move(clonedArgs));
}

size_t FunctionCall::hash() const {
  size_t h = std::hash<std::string>{}(name_);
  for (const ExprNode* arg : arguments_) {
    h = combineHash(h, arg->hash());
  }
  return h;
}

bool FunctionCall::equals(const ExprNode& other) const {
  if (other.type() != NodeType::FunctionCall)
    return false;
  const auto& o = static_cast<const FunctionCall&>(other);
  if (name_ != o.name_ || arguments_.size() != o.arguments_.size())
    return false;
  for (size_t i = 0; i < arguments_.size(); ++i) {
    if (!arguments_[i]->equals(*o.arguments_[i]))
      return false;
  }
  return true;
}

// ArrayIndex

void ArrayIndex::accept(ExprVisitor& visitor) {
  visitor.visit(*this);
}

void ArrayIndex::accept(ConstExprVisitor& visitor) const {
  visitor.visit(*this);
}

ExprNode* ArrayIndex::clone(ExprArena& arena) const {
  return arena.make<ArrayIndex>(array_->clone(arena), index_->clone(arena));
}

size_t ArrayIndex::hash() const {
  size_t h = array_->hash();
  h = combineHash(h, index_->hash());
  return h;
}

bool ArrayIndex::equals(const ExprNode& other) const {
  if (other.type() != NodeType::ArrayIndex)
    return false;
  const auto& o = static_cast<const ArrayIndex&>(other);
  return array_->equals(*o.array_) && index_->equals(*o.index_);
}

// TernaryConditional

void TernaryConditional::accept(ExprVisitor& visitor) {
  visitor.visit(*this);
}

void TernaryConditional::accept(ConstExprVisitor& visitor) const {
  visitor.visit(*this);
}

ExprNode* TernaryConditional::clone(ExprArena& arena) const {
  return arena.make<TernaryConditional>(condition_->clone(arena), trueExpr_->clone(arena),
                                        falseExpr_->clone(arena));
}

size_t TernaryConditional::hash() const {
  size_t h = condition_->hash();
  h = combineHash(h, trueExpr_->hash());
  h = combineHash(h, falseExpr_->hash());
  return h;
}

bool TernaryConditional::equals(const ExprNode& other) const {
  if (other.type() != NodeType::TernaryConditional)
    return false;
  const auto& o = static_cast<const TernaryConditional&>(other);
  return condition_->equals(*o.condition_) && trueExpr_->equals(*o.trueExpr_) &&
         falseExpr_->equals(*o.falseExpr_);
}

}  // namespace spice_expr
