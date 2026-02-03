#ifndef SPICE_EXPR_VISITOR_DIFFERENTIATOR_H
#define SPICE_EXPR_VISITOR_DIFFERENTIATOR_H

#include <string>
#include <unordered_set>
#include <variant>

#include "spice_expr/visitor/visitor.h"

namespace spice_expr {

class ExprArena;
class ExprNode;
class SymbolTable;

class DiffTarget {
 public:
  struct Parameter {
    std::string name;
    explicit Parameter(std::string n) : name(std::move(n)) {}
  };

  struct NodeVoltage {
    std::string node;
    explicit NodeVoltage(std::string n) : node(std::move(n)) {}
  };

  struct DeviceCurrent {
    std::string device;
    explicit DeviceCurrent(std::string d) : device(std::move(d)) {}
  };

  static DiffTarget parameter(const std::string& name) { return DiffTarget(Parameter(name)); }

  static DiffTarget node_voltage(const std::string& node) { return DiffTarget(NodeVoltage(node)); }

  static DiffTarget device_current(const std::string& device) {
    return DiffTarget(DeviceCurrent(device));
  }

  bool is_parameter() const { return std::holds_alternative<Parameter>(target_); }
  bool is_node_voltage() const { return std::holds_alternative<NodeVoltage>(target_); }
  bool is_device_current() const { return std::holds_alternative<DeviceCurrent>(target_); }

  const std::string& name() const;

 private:
  explicit DiffTarget(std::variant<Parameter, NodeVoltage, DeviceCurrent> t)
      : target_(std::move(t)) {}

  std::variant<Parameter, NodeVoltage, DeviceCurrent> target_;
};

class Differentiator : public ConstExprVisitor {
 public:
  Differentiator(ExprArena& arena, DiffTarget target);
  Differentiator(ExprArena& arena, DiffTarget target, const SymbolTable* symbols);

  ExprNode* differentiate(const ExprNode& node);

  void visit(const NumberLiteral& node) override;
  void visit(const StringLiteral& node) override;
  void visit(const ArrayLiteral& node) override;
  void visit(const Identifier& node) override;
  void visit(const CircuitNodeRef& node) override;
  void visit(const CircuitCurrentRef& node) override;
  void visit(const BinaryOp& node) override;
  void visit(const UnaryOp& node) override;
  void visit(const FunctionCall& node) override;
  void visit(const ArrayIndex& node) override;
  void visit(const TernaryConditional& node) override;

 private:
  ExprNode* zero();
  ExprNode* one();
  ExprNode* clone_node(const ExprNode& node);

  ExprArena& arena_;
  DiffTarget target_;
  ExprNode* result_;
  const SymbolTable* symbols_ = nullptr;
  std::unordered_set<std::string> visiting_;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_VISITOR_DIFFERENTIATOR_H
