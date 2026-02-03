#ifndef SPICE_EXPR_PARSER_LEXER_H
#define SPICE_EXPR_PARSER_LEXER_H

#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace spice_expr {

enum class TokenType {
  Number,
  Identifier,
  String,
  Plus,
  Minus,
  Star,
  Slash,
  Percent,
  Power,
  LParen,
  RParen,
  LBracket,
  RBracket,
  LBrace,
  RBrace,
  Comma,
  Question,
  Colon,
  Less,
  LessEqual,
  Greater,
  GreaterEqual,
  Equal,
  NotEqual,
  And,
  Or,
  Not,
  EndOfInput,
  Error
};

const char* tokenTypeToString(TokenType type);

struct Token {
  TokenType type;
  std::string text;
  int line;
  int column;
  std::variant<std::monostate, double, std::pair<double, double>> value;

  Token(TokenType t, std::string txt, int l, int c)
      : type(t), text(std::move(txt)), line(l), column(c) {}

  bool is_number() const { return type == TokenType::Number; }
  bool is_identifier() const { return type == TokenType::Identifier; }
  bool is_operator() const;
  bool is_end_of_input() const { return type == TokenType::EndOfInput; }
  bool is_error() const { return type == TokenType::Error; }

  double number_value() const;
  std::pair<double, double> complex_value() const;
  bool is_complex_number() const;
};

class Lexer {
 public:
  explicit Lexer(const std::string& input);

  Token next_token();
  Token peek_token();
  bool has_more() const;

  int current_line() const { return line_; }
  int current_column() const { return column_; }

 private:
  void skip_whitespace();
  char peek() const;
  char advance();
  bool match(char expected);
  bool is_at_end() const;

  Token scan_number();
  Token scan_identifier();
  Token scan_string();
  Token make_token(TokenType type);
  Token error_token(const std::string& message);

  double parse_engineering_multiplier(const std::string& suffix) const;

  std::string input_;
  size_t pos_;
  int line_;
  int column_;
  std::optional<Token> peeked_;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_PARSER_LEXER_H
