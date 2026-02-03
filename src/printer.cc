#include "spice_expr/visitor/printer.h"

#include <cmath>
#include <iomanip>

#include "spice_expr/ast/functions.h"
#include "spice_expr/ast/literals.h"
#include "spice_expr/ast/operations.h"
#include "spice_expr/ast/references.h"

namespace spice_expr {

ExprPrinter::ExprPrinter(PrintFormat format) : format_(format) {}

std::string ExprPrinter::print(const ExprNode& node) {
  ss_.str("");
  ss_.clear();
  node.accept(*this);
  return ss_.str();
}

void ExprPrinter::visit(const NumberLiteral& node) {
  if (node.is_complex()) {
    if (format_ == PrintFormat::LaTeX) {
      ss_ << node.real() << " + " << node.imag() << "j";
    } else {
      ss_ << "(" << node.real() << ", " << node.imag() << ")";
    }
  } else {
    double val = node.real();
    if (val == std::floor(val) && std::abs(val) < 1e10) {
      ss_ << static_cast<long long>(val);
    } else {
      ss_ << val;
    }
  }
}

void ExprPrinter::visit(const StringLiteral& node) {
  ss_ << "\"" << node.value() << "\"";
}

void ExprPrinter::visit(const ArrayLiteral& node) {
  ss_ << "[";
  bool first = true;
  for (const auto* elem : node.elements()) {
    if (!first)
      ss_ << ", ";
    first = false;
    elem->accept(*this);
  }
  ss_ << "]";
}

void ExprPrinter::visit(const Identifier& node) {
  ss_ << node.name();
}

void ExprPrinter::visit(const CircuitNodeRef& node) {
  if (format_ == PrintFormat::LaTeX) {
    if (node.is_differential()) {
      ss_ << "V_{" << node.node1() << "," << node.node2() << "}";
    } else {
      ss_ << "V_{" << node.node1() << "}";
    }
  } else {
    if (node.is_differential()) {
      ss_ << "V(" << node.node1() << ", " << node.node2() << ")";
    } else {
      ss_ << "V(" << node.node1() << ")";
    }
  }
}

void ExprPrinter::visit(const CircuitCurrentRef& node) {
  if (format_ == PrintFormat::LaTeX) {
    ss_ << "I_{" << node.device() << "}";
  } else {
    ss_ << "I(" << node.device() << ")";
  }
}

void ExprPrinter::visit(const BinaryOp& node) {
  bool needLeftParen = needs_parens(*node.left(), node, false);
  bool needRightParen = needs_parens(*node.right(), node, true);

  if (needLeftParen)
    ss_ << "(";
  node.left()->accept(*this);
  if (needLeftParen)
    ss_ << ")";

  if (format_ == PrintFormat::LaTeX) {
    switch (node.op_type()) {
      case BinaryOpType::Multiply:
        ss_ << " \\cdot ";
        break;
      case BinaryOpType::Divide:
        ss_ << " / ";
        break;
      case BinaryOpType::Power:
        ss_ << "^";
        break;
      case BinaryOpType::LessEqual:
        ss_ << " \\leq ";
        break;
      case BinaryOpType::GreaterEqual:
        ss_ << " \\geq ";
        break;
      case BinaryOpType::NotEqual:
        ss_ << " \\neq ";
        break;
      case BinaryOpType::LogicalAnd:
        ss_ << " \\land ";
        break;
      case BinaryOpType::LogicalOr:
        ss_ << " \\lor ";
        break;
      default:
        ss_ << " " << binaryOpTypeToString(node.op_type()) << " ";
        break;
    }
  } else {
    ss_ << " " << binaryOpTypeToString(node.op_type()) << " ";
  }

  if (needRightParen)
    ss_ << "(";
  node.right()->accept(*this);
  if (needRightParen)
    ss_ << ")";
}

void ExprPrinter::visit(const UnaryOp& node) {
  ss_ << unaryOpTypeToString(node.op_type());
  bool needParen = (node.operand()->type() == NodeType::BinaryOp ||
                    node.operand()->type() == NodeType::TernaryConditional);
  if (needParen)
    ss_ << "(";
  node.operand()->accept(*this);
  if (needParen)
    ss_ << ")";
}

void ExprPrinter::visit(const FunctionCall& node) {
  if (format_ == PrintFormat::LaTeX) {
    std::string name = node.name();
    if (name == "sin" || name == "cos" || name == "tan" || name == "log" || name == "exp" ||
        name == "sqrt") {
      ss_ << "\\" << name << "(";
    } else {
      ss_ << "\\text{" << name << "}(";
    }
  } else {
    ss_ << node.name() << "(";
  }

  bool first = true;
  for (const auto* arg : node.arguments()) {
    if (!first)
      ss_ << ", ";
    first = false;
    arg->accept(*this);
  }
  ss_ << ")";
}

void ExprPrinter::visit(const ArrayIndex& node) {
  node.array()->accept(*this);
  ss_ << "[";
  node.index()->accept(*this);
  ss_ << "]";
}

void ExprPrinter::visit(const TernaryConditional& node) {
  node.condition()->accept(*this);
  ss_ << " ? ";
  node.true_expr()->accept(*this);
  ss_ << " : ";
  node.false_expr()->accept(*this);
}

int ExprPrinter::precedence(const ExprNode& node) const {
  if (node.type() == NodeType::BinaryOp) {
    switch (static_cast<const BinaryOp&>(node).op_type()) {
      case BinaryOpType::LogicalOr:
        return 1;
      case BinaryOpType::LogicalAnd:
        return 2;
      case BinaryOpType::Equal:
      case BinaryOpType::NotEqual:
        return 3;
      case BinaryOpType::Less:
      case BinaryOpType::LessEqual:
      case BinaryOpType::Greater:
      case BinaryOpType::GreaterEqual:
        return 4;
      case BinaryOpType::Add:
      case BinaryOpType::Subtract:
        return 5;
      case BinaryOpType::Multiply:
      case BinaryOpType::Divide:
      case BinaryOpType::Modulo:
        return 6;
      case BinaryOpType::Power:
        return 7;
    }
  }
  if (node.type() == NodeType::UnaryOp)
    return 8;
  if (node.type() == NodeType::TernaryConditional)
    return 0;
  return 10;
}

bool ExprPrinter::needs_parens(const ExprNode& child, const ExprNode& parent, bool isRight) const {
  int childPrec = precedence(child);
  int parentPrec = precedence(parent);

  if (childPrec < parentPrec)
    return true;
  if (childPrec > parentPrec)
    return false;

  if (parent.type() == NodeType::BinaryOp) {
    auto op = static_cast<const BinaryOp&>(parent).op_type();
    if (isRight) {
      if (op == BinaryOpType::Subtract || op == BinaryOpType::Divide) {
        return true;
      }
      if (op == BinaryOpType::Power) {
        return false;
      }
    } else {
      // Left operand: power is right-associative, so (a^b)^c needs parens on the left
      if (op == BinaryOpType::Power && child.type() == NodeType::BinaryOp) {
        auto childOp = static_cast<const BinaryOp&>(child).op_type();
        if (childOp == BinaryOpType::Power) {
          return true;
        }
      }
    }
  }
  return false;
}

}  // namespace spice_expr
