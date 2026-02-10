#include "spice_expr/parser/parser.h"

#include <algorithm>
#include <cctype>

#include "spice_expr/ast/arena.h"
#include "spice_expr/ast/functions.h"
#include "spice_expr/ast/literals.h"
#include "spice_expr/ast/operations.h"
#include "spice_expr/ast/references.h"

namespace spice_expr {

namespace {

// Returns right binding power for prefix operators, or 0 if not prefix.
int prefix_bp(TokenType type) {
  switch (type) {
    case TokenType::Minus:
    case TokenType::Plus:
    case TokenType::Not:
      return 17;
    default:
      return 0;
  }
}

// Returns {left_bp, right_bp} for infix operators, or {0, 0} if not infix.
std::pair<int, int> infix_bp(TokenType type) {
  switch (type) {
    case TokenType::Question:
      return {2, 1};
    case TokenType::Or:
      return {3, 4};
    case TokenType::And:
      return {5, 6};
    case TokenType::Equal:
    case TokenType::NotEqual:
      return {7, 8};
    case TokenType::Less:
    case TokenType::LessEqual:
    case TokenType::Greater:
    case TokenType::GreaterEqual:
      return {9, 10};
    case TokenType::Plus:
    case TokenType::Minus:
      return {11, 12};
    case TokenType::Star:
    case TokenType::Slash:
    case TokenType::Percent:
      return {13, 14};
    case TokenType::Power:
      return {16, 15};
    default:
      return {0, 0};
  }
}

// Returns left binding power for postfix operators, or 0 if not postfix.
int postfix_bp(TokenType type) {
  switch (type) {
    case TokenType::LBracket:
      return 19;
    default:
      return 0;
  }
}

// Maps infix TokenType to BinaryOpType.
BinaryOpType token_to_binop(TokenType type) {
  switch (type) {
    case TokenType::Or:
      return BinaryOpType::LogicalOr;
    case TokenType::And:
      return BinaryOpType::LogicalAnd;
    case TokenType::Equal:
      return BinaryOpType::Equal;
    case TokenType::NotEqual:
      return BinaryOpType::NotEqual;
    case TokenType::Less:
      return BinaryOpType::Less;
    case TokenType::LessEqual:
      return BinaryOpType::LessEqual;
    case TokenType::Greater:
      return BinaryOpType::Greater;
    case TokenType::GreaterEqual:
      return BinaryOpType::GreaterEqual;
    case TokenType::Plus:
      return BinaryOpType::Add;
    case TokenType::Minus:
      return BinaryOpType::Subtract;
    case TokenType::Star:
      return BinaryOpType::Multiply;
    case TokenType::Slash:
      return BinaryOpType::Divide;
    case TokenType::Percent:
      return BinaryOpType::Modulo;
    case TokenType::Power:
      return BinaryOpType::Power;
    default:
      return BinaryOpType::Add;  // unreachable
  }
}

}  // namespace

Parser::Parser(const std::string& input, ExprArena& arena)
    : lexer_(input), arena_(arena), current_(TokenType::Error, "", 0, 0), hasError_(false) {
  current_ = lexer_.next_token();
}

ExprNode* Parser::parse_expression() {
  return expression();
}

ExprNode* Parser::expression() {
  return expr_bp(0);
}

ExprNode* Parser::expr_bp(int min_bp) {
  // --- Prefix / atom ---
  ExprNode* lhs;
  int r_bp = prefix_bp(current_.type);
  if (r_bp > 0) {
    TokenType op = current_.type;
    advance();
    ExprNode* rhs = expr_bp(r_bp);
    if (op == TokenType::Minus)
      lhs = arena_.make<UnaryOp>(UnaryOpType::Negate, rhs);
    else if (op == TokenType::Not)
      lhs = arena_.make<UnaryOp>(UnaryOpType::LogicalNot, rhs);
    else  // unary +
      lhs = rhs;
  } else {
    lhs = primary();
  }

  // --- Postfix / infix loop ---
  for (;;) {
    // Postfix: array indexing
    int post_bp = postfix_bp(current_.type);
    if (post_bp > 0) {
      if (post_bp < min_bp) break;
      advance();  // consume '['
      ExprNode* index = expr_bp(0);
      consume(TokenType::RBracket, "Expected ']' after array index");
      lhs = arena_.make<ArrayIndex>(lhs, index);
      continue;
    }

    // Infix
    auto [l_bp, r_bp2] = infix_bp(current_.type);
    if (l_bp == 0 || l_bp < min_bp) break;

    TokenType op = current_.type;
    advance();

    if (op == TokenType::Question) {
      // Ternary: middle resets to bp=0, right uses r_bp
      ExprNode* mhs = expr_bp(0);
      consume(TokenType::Colon, "Expected ':' in ternary expression");
      ExprNode* rhs = expr_bp(r_bp2);
      lhs = arena_.make<TernaryConditional>(lhs, mhs, rhs);
    } else {
      ExprNode* rhs = expr_bp(r_bp2);
      lhs = arena_.make<BinaryOp>(token_to_binop(op), lhs, rhs);
    }
  }

  return lhs;
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
