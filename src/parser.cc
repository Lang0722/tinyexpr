#include "spice_expr/parser/parser.h"

#include <algorithm>
#include <cctype>

#include "spice_expr/ast/arena.h"
#include "spice_expr/ast/functions.h"
#include "spice_expr/ast/literals.h"
#include "spice_expr/ast/operations.h"
#include "spice_expr/ast/references.h"

namespace spice_expr {

Parser::Parser(const std::string& input, ExprArena& arena)
    : lexer_(input), arena_(arena), current_(TokenType::Error, "", 0, 0), hasError_(false) {
  current_ = lexer_.next_token();
}

ExprNode* Parser::parse_expression() {
  return expression();
}

ExprNode* Parser::expression() {
  return ternary();
}

ExprNode* Parser::ternary() {
  ExprNode* expr = logical_or();

  if (match(TokenType::Question)) {
    ExprNode* trueExpr = expression();
    consume(TokenType::Colon, "Expected ':' in ternary expression");
    ExprNode* falseExpr = expression();
    expr = arena_.make<TernaryConditional>(expr, trueExpr, falseExpr);
  }

  return expr;
}

ExprNode* Parser::logical_or() {
  ExprNode* expr = logical_and();

  while (match(TokenType::Or)) {
    ExprNode* right = logical_and();
    expr = arena_.make<BinaryOp>(BinaryOpType::LogicalOr, expr, right);
  }

  return expr;
}

ExprNode* Parser::logical_and() {
  ExprNode* expr = equality();

  while (match(TokenType::And)) {
    ExprNode* right = equality();
    expr = arena_.make<BinaryOp>(BinaryOpType::LogicalAnd, expr, right);
  }

  return expr;
}

ExprNode* Parser::equality() {
  ExprNode* expr = comparison();

  while (true) {
    if (match(TokenType::Equal)) {
      ExprNode* right = comparison();
      expr = arena_.make<BinaryOp>(BinaryOpType::Equal, expr, right);
    } else if (match(TokenType::NotEqual)) {
      ExprNode* right = comparison();
      expr = arena_.make<BinaryOp>(BinaryOpType::NotEqual, expr, right);
    } else {
      break;
    }
  }

  return expr;
}

ExprNode* Parser::comparison() {
  ExprNode* expr = additive();

  while (true) {
    if (match(TokenType::Less)) {
      ExprNode* right = additive();
      expr = arena_.make<BinaryOp>(BinaryOpType::Less, expr, right);
    } else if (match(TokenType::LessEqual)) {
      ExprNode* right = additive();
      expr = arena_.make<BinaryOp>(BinaryOpType::LessEqual, expr, right);
    } else if (match(TokenType::Greater)) {
      ExprNode* right = additive();
      expr = arena_.make<BinaryOp>(BinaryOpType::Greater, expr, right);
    } else if (match(TokenType::GreaterEqual)) {
      ExprNode* right = additive();
      expr = arena_.make<BinaryOp>(BinaryOpType::GreaterEqual, expr, right);
    } else {
      break;
    }
  }

  return expr;
}

ExprNode* Parser::additive() {
  ExprNode* expr = multiplicative();

  while (true) {
    if (match(TokenType::Plus)) {
      ExprNode* right = multiplicative();
      expr = arena_.make<BinaryOp>(BinaryOpType::Add, expr, right);
    } else if (match(TokenType::Minus)) {
      ExprNode* right = multiplicative();
      expr = arena_.make<BinaryOp>(BinaryOpType::Subtract, expr, right);
    } else {
      break;
    }
  }

  return expr;
}

ExprNode* Parser::multiplicative() {
  ExprNode* expr = power();

  while (true) {
    if (match(TokenType::Star)) {
      ExprNode* right = power();
      expr = arena_.make<BinaryOp>(BinaryOpType::Multiply, expr, right);
    } else if (match(TokenType::Slash)) {
      ExprNode* right = power();
      expr = arena_.make<BinaryOp>(BinaryOpType::Divide, expr, right);
    } else if (match(TokenType::Percent)) {
      ExprNode* right = power();
      expr = arena_.make<BinaryOp>(BinaryOpType::Modulo, expr, right);
    } else {
      break;
    }
  }

  return expr;
}

ExprNode* Parser::power() {
  ExprNode* expr = unary();

  if (match(TokenType::Power)) {
    ExprNode* right = power();
    expr = arena_.make<BinaryOp>(BinaryOpType::Power, expr, right);
  }

  return expr;
}

ExprNode* Parser::unary() {
  if (match(TokenType::Minus)) {
    ExprNode* operand = unary();
    return arena_.make<UnaryOp>(UnaryOpType::Negate, operand);
  }

  if (match(TokenType::Plus)) {
    return unary();
  }

  if (match(TokenType::Not)) {
    ExprNode* operand = unary();
    return arena_.make<UnaryOp>(UnaryOpType::LogicalNot, operand);
  }

  return postfix();
}

ExprNode* Parser::postfix() {
  ExprNode* expr = primary();

  while (true) {
    if (match(TokenType::LBracket)) {
      ExprNode* index = expression();
      consume(TokenType::RBracket, "Expected ']' after array index");
      expr = arena_.make<ArrayIndex>(expr, index);
    } else {
      break;
    }
  }

  return expr;
}

ExprNode* Parser::primary() {
  if (check(TokenType::Number)) {
    double val = current_.number_value();
    advance();
    return arena_.make<NumberLiteral>(val);
  }

  if (check(TokenType::Identifier)) {
    std::string name = current_.text;
    advance();

    std::string lower = name;
    std::transform(lower.begin(), lower.end(), lower.begin(),
                   [](unsigned char c) { return std::tolower(c); });

    if (lower == "v" && check(TokenType::LParen)) {
      return circuit_ref("V");
    }
    if (lower == "i" && check(TokenType::LParen)) {
      return circuit_ref("I");
    }

    if (check(TokenType::LParen)) {
      return function_call(name);
    }

    return arena_.make<Identifier>(name);
  }

  if (check(TokenType::String)) {
    std::string val = current_.text;
    advance();
    return arena_.make<StringLiteral>(val);
  }

  if (match(TokenType::LParen)) {
    ExprNode* expr = expression();

    // Check if this is a complex literal: (real, imag)
    if (match(TokenType::Comma)) {
      if (expr->type() != NodeType::NumberLiteral) {
        error("Expected number for real part of complex literal");
        consume(TokenType::RParen, "");
        return arena_.make<NumberLiteral>(0.0);
      }
      double real = static_cast<NumberLiteral*>(expr)->real();

      ExprNode* imagExpr = expression();
      if (imagExpr->type() != NodeType::NumberLiteral) {
        error("Expected number for imaginary part of complex literal");
        consume(TokenType::RParen, "");
        return arena_.make<NumberLiteral>(0.0);
      }
      double imag = static_cast<NumberLiteral*>(imagExpr)->real();

      consume(TokenType::RParen, "Expected ')' after complex number");
      return arena_.make<NumberLiteral>(real, imag);
    }

    consume(TokenType::RParen, "Expected ')' after expression");
    return expr;
  }

  if (match(TokenType::LBracket)) {
    return array_literal();
  }

  error("Expected expression");
  return arena_.make<NumberLiteral>(0.0);
}

ExprNode* Parser::function_call(const std::string& name) {
  consume(TokenType::LParen, "Expected '(' after function name");

  std::vector<ExprNode*> args;
  if (!check(TokenType::RParen)) {
    do {
      args.push_back(expression());
    } while (match(TokenType::Comma));
  }

  consume(TokenType::RParen, "Expected ')' after function arguments");
  return arena_.make<FunctionCall>(name, std::move(args));
}

ExprNode* Parser::array_literal() {
  std::vector<ExprNode*> elements;

  if (!check(TokenType::RBracket)) {
    do {
      elements.push_back(expression());
    } while (match(TokenType::Comma));
  }

  consume(TokenType::RBracket, "Expected ']' after array elements");
  return arena_.make<ArrayLiteral>(std::move(elements));
}

ExprNode* Parser::circuit_ref(const std::string& func) {
  consume(TokenType::LParen, "Expected '(' after V or I");

  if (!check(TokenType::Identifier) && !check(TokenType::Number)) {
    error("Expected node or device name");
    consume(TokenType::RParen, "");
    return arena_.make<NumberLiteral>(0.0);
  }

  std::string node1 = current_.text;
  advance();

  if (func == "V" && match(TokenType::Comma)) {
    if (!check(TokenType::Identifier) && !check(TokenType::Number)) {
      error("Expected second node name");
      consume(TokenType::RParen, "");
      return arena_.make<NumberLiteral>(0.0);
    }
    std::string node2 = current_.text;
    advance();
    consume(TokenType::RParen, "Expected ')' after node names");
    return arena_.make<CircuitNodeRef>(node1, node2);
  }

  consume(TokenType::RParen, "Expected ')' after node/device name");

  if (func == "V") {
    return arena_.make<CircuitNodeRef>(node1);
  } else {
    return arena_.make<CircuitCurrentRef>(node1);
  }
}

Token Parser::advance() {
  Token prev = current_;
  current_ = lexer_.next_token();
  return prev;
}

Token Parser::peek() {
  return current_;
}

bool Parser::check(TokenType type) {
  return current_.type == type;
}

bool Parser::match(TokenType type) {
  if (check(type)) {
    advance();
    return true;
  }
  return false;
}

Token Parser::consume(TokenType type, const std::string& message) {
  if (check(type)) {
    return advance();
  }

  error(message + " (got '" + current_.text + "')");
  return current_;
}

void Parser::error(const std::string& message) {
  hasError_ = true;
  errors_.push_back("Line " + std::to_string(current_.line) + ", column " +
                    std::to_string(current_.column) + ": " + message);
  structuredErrors_.push_back({message, current_.line, current_.column});
}

void Parser::synchronize() {
  advance();

  while (!check(TokenType::EndOfInput)) {
    if (check(TokenType::RParen) || check(TokenType::RBracket) || check(TokenType::Comma)) {
      return;
    }
    advance();
  }
}

ExprNode* parse_expression(const std::string& input, ExprArena& arena) {
  Parser parser(input, arena);
  ExprNode* result = parser.parse_expression();
  if (parser.has_error()) {
    const auto& err = parser.structured_errors().front();
    throw ParseError(err.message, err.line, err.column);
  }
  return result;
}

}  // namespace spice_expr
