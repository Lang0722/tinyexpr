#ifndef SPICE_EXPR_PARSER_PARSER_H
#define SPICE_EXPR_PARSER_PARSER_H

#include <stdexcept>
#include <string>
#include <vector>

#include "spice_expr/parser/lexer.h"

namespace spice_expr {

class ExprArena;
class ExprNode;

class ParseError : public std::runtime_error {
 public:
  ParseError(const std::string& msg, int line, int column)
      : std::runtime_error(msg), line_(line), column_(column) {}

  int line() const { return line_; }
  int column() const { return column_; }

 private:
  int line_;
  int column_;
};

struct ParserError {
  std::string message;
  int line;
  int column;
};

class Parser {
 public:
  Parser(const std::string& input, ExprArena& arena);

  ExprNode* parse_expression();
  bool has_error() const { return hasError_; }
  const std::vector<std::string>& errors() const { return errors_; }
  const std::vector<ParserError>& structured_errors() const { return structuredErrors_; }

 private:
  ExprNode* expression();
  ExprNode* ternary();
  ExprNode* logical_or();
  ExprNode* logical_and();
  ExprNode* equality();
  ExprNode* comparison();
  ExprNode* additive();
  ExprNode* multiplicative();
  ExprNode* power();
  ExprNode* unary();
  ExprNode* postfix();
  ExprNode* primary();

  ExprNode* function_call(const std::string& name);
  ExprNode* array_literal();
  ExprNode* circuit_ref(const std::string& func);

  Token advance();
  bool check(TokenType type);
  bool match(TokenType type);
  Token consume(TokenType type, const std::string& message);

  void error(const std::string& message);

  Lexer lexer_;
  ExprArena& arena_;
  Token current_;
  bool hasError_;
  std::vector<std::string> errors_;
  std::vector<ParserError> structuredErrors_;
};

ExprNode* parse_expression(const std::string& input, ExprArena& arena);

}  // namespace spice_expr

#endif  // SPICE_EXPR_PARSER_PARSER_H
