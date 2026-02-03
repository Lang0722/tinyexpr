#include "spice_expr/parser/lexer.h"

#include <algorithm>
#include <cctype>
#include <cmath>

namespace spice_expr {

const char* tokenTypeToString(TokenType type) {
  switch (type) {
    case TokenType::Number:
      return "Number";
    case TokenType::Identifier:
      return "Identifier";
    case TokenType::String:
      return "String";
    case TokenType::Plus:
      return "+";
    case TokenType::Minus:
      return "-";
    case TokenType::Star:
      return "*";
    case TokenType::Slash:
      return "/";
    case TokenType::Percent:
      return "%";
    case TokenType::Power:
      return "**";
    case TokenType::LParen:
      return "(";
    case TokenType::RParen:
      return ")";
    case TokenType::LBracket:
      return "[";
    case TokenType::RBracket:
      return "]";
    case TokenType::LBrace:
      return "{";
    case TokenType::RBrace:
      return "}";
    case TokenType::Comma:
      return ",";
    case TokenType::Question:
      return "?";
    case TokenType::Colon:
      return ":";
    case TokenType::Less:
      return "<";
    case TokenType::LessEqual:
      return "<=";
    case TokenType::Greater:
      return ">";
    case TokenType::GreaterEqual:
      return ">=";
    case TokenType::Equal:
      return "==";
    case TokenType::NotEqual:
      return "!=";
    case TokenType::And:
      return "&&";
    case TokenType::Or:
      return "||";
    case TokenType::Not:
      return "!";
    case TokenType::EndOfInput:
      return "EOF";
    case TokenType::Error:
      return "Error";
  }
  return "Unknown";
}

bool Token::is_operator() const {
  switch (type) {
    case TokenType::Plus:
    case TokenType::Minus:
    case TokenType::Star:
    case TokenType::Slash:
    case TokenType::Percent:
    case TokenType::Power:
    case TokenType::Less:
    case TokenType::LessEqual:
    case TokenType::Greater:
    case TokenType::GreaterEqual:
    case TokenType::Equal:
    case TokenType::NotEqual:
    case TokenType::And:
    case TokenType::Or:
    case TokenType::Not:
      return true;
    default:
      return false;
  }
}

double Token::number_value() const {
  if (std::holds_alternative<double>(value)) {
    return std::get<double>(value);
  }
  if (std::holds_alternative<std::pair<double, double>>(value)) {
    return std::get<std::pair<double, double>>(value).first;
  }
  return 0.0;
}

std::pair<double, double> Token::complex_value() const {
  if (std::holds_alternative<std::pair<double, double>>(value)) {
    return std::get<std::pair<double, double>>(value);
  }
  if (std::holds_alternative<double>(value)) {
    return {std::get<double>(value), 0.0};
  }
  return {0.0, 0.0};
}

bool Token::is_complex_number() const {
  return std::holds_alternative<std::pair<double, double>>(value);
}

Lexer::Lexer(const std::string& input) : input_(input), pos_(0), line_(1), column_(1) {}

Token Lexer::next_token() {
  if (peeked_) {
    Token t = std::move(*peeked_);
    peeked_.reset();
    return t;
  }

  skip_whitespace();

  if (is_at_end()) {
    return make_token(TokenType::EndOfInput);
  }

  char c = advance();

  if (std::isdigit(c) || (c == '.' && !is_at_end() && std::isdigit(peek()))) {
    pos_--;
    column_--;
    return scan_number();
  }

  if (std::isalpha(c) || c == '_') {
    pos_--;
    column_--;
    return scan_identifier();
  }

  if (c == '"' || c == '\'') {
    pos_--;
    column_--;
    return scan_string();
  }

  switch (c) {
    case '+':
      return make_token(TokenType::Plus);
    case '-':
      return make_token(TokenType::Minus);
    case '/':
      return make_token(TokenType::Slash);
    case '%':
      return make_token(TokenType::Percent);
    case '(':
      return make_token(TokenType::LParen);
    case ')':
      return make_token(TokenType::RParen);
    case '[':
      return make_token(TokenType::LBracket);
    case ']':
      return make_token(TokenType::RBracket);
    case '{':
      return make_token(TokenType::LBrace);
    case '}':
      return make_token(TokenType::RBrace);
    case ',':
      return make_token(TokenType::Comma);
    case '?':
      return make_token(TokenType::Question);
    case ':':
      return make_token(TokenType::Colon);

    case '*':
      if (match('*')) {
        Token t = make_token(TokenType::Power);
        t.text = "**";
        return t;
      }
      return make_token(TokenType::Star);

    case '<':
      if (match('=')) {
        Token t = make_token(TokenType::LessEqual);
        t.text = "<=";
        return t;
      }
      return make_token(TokenType::Less);

    case '>':
      if (match('=')) {
        Token t = make_token(TokenType::GreaterEqual);
        t.text = ">=";
        return t;
      }
      return make_token(TokenType::Greater);

    case '=':
      if (match('=')) {
        Token t = make_token(TokenType::Equal);
        t.text = "==";
        return t;
      }
      return error_token("Expected '=' after '='");

    case '!':
      if (match('=')) {
        Token t = make_token(TokenType::NotEqual);
        t.text = "!=";
        return t;
      }
      return make_token(TokenType::Not);

    case '&':
      if (match('&')) {
        Token t = make_token(TokenType::And);
        t.text = "&&";
        return t;
      }
      return error_token("Expected '&' after '&'");

    case '|':
      if (match('|')) {
        Token t = make_token(TokenType::Or);
        t.text = "||";
        return t;
      }
      return error_token("Expected '|' after '|'");
  }

  return error_token(std::string("Unexpected character: ") + c);
}

Token Lexer::peek_token() {
  if (!peeked_) {
    peeked_ = next_token();
  }
  return *peeked_;
}

bool Lexer::has_more() const {
  return !is_at_end() || peeked_.has_value();
}

void Lexer::skip_whitespace() {
  while (!is_at_end()) {
    char c = peek();
    if (c == ' ' || c == '\t' || c == '\r') {
      advance();
    } else if (c == '\n') {
      advance();
      line_++;
      column_ = 1;
    } else if (c == '$' || c == ';') {
      while (!is_at_end() && peek() != '\n') {
        advance();
      }
    } else {
      break;
    }
  }
}

char Lexer::peek() const {
  if (is_at_end())
    return '\0';
  return input_[pos_];
}

char Lexer::advance() {
  column_++;
  return input_[pos_++];
}

bool Lexer::match(char expected) {
  if (is_at_end())
    return false;
  if (input_[pos_] != expected)
    return false;
  pos_++;
  column_++;
  return true;
}

bool Lexer::is_at_end() const {
  return pos_ >= input_.size();
}

Token Lexer::scan_number() {
  int startLine = line_;
  int startCol = column_;
  size_t start = pos_;

  while (!is_at_end() && std::isdigit(peek())) {
    advance();
  }

  if (!is_at_end() && peek() == '.' && pos_ + 1 < input_.size() && std::isdigit(input_[pos_ + 1])) {
    advance();
    while (!is_at_end() && std::isdigit(peek())) {
      advance();
    }
  }

  if (!is_at_end() && (peek() == 'e' || peek() == 'E')) {
    size_t savedPos = pos_;
    int savedCol = column_;
    advance();
    if (!is_at_end() && (peek() == '+' || peek() == '-')) {
      advance();
    }
    if (!is_at_end() && std::isdigit(peek())) {
      while (!is_at_end() && std::isdigit(peek())) {
        advance();
      }
    } else {
      pos_ = savedPos;
      column_ = savedCol;
    }
  }

  std::string numStr = input_.substr(start, pos_ - start);
  double value = std::stod(numStr);

  std::string suffix;
  size_t suffixStart = pos_;
  while (!is_at_end() && std::isalpha(peek())) {
    suffix += advance();
  }

  if (!suffix.empty()) {
    double multiplier = parse_engineering_multiplier(suffix);
    if (multiplier != 1.0) {
      value *= multiplier;
    } else {
      pos_ = suffixStart;
      column_ -= static_cast<int>(suffix.size());
    }
  }

  Token t(TokenType::Number, input_.substr(start, pos_ - start), startLine, startCol);
  t.value = value;
  return t;
}

Token Lexer::scan_identifier() {
  int startLine = line_;
  int startCol = column_;
  size_t start = pos_;

  while (!is_at_end() && (std::isalnum(peek()) || peek() == '_')) {
    advance();
  }

  std::string text = input_.substr(start, pos_ - start);
  return Token(TokenType::Identifier, text, startLine, startCol);
}

Token Lexer::scan_string() {
  int startLine = line_;
  int startCol = column_;
  char quote = advance();

  std::string value;
  while (!is_at_end() && peek() != quote) {
    if (peek() == '\\' && pos_ + 1 < input_.size()) {
      advance();
      char escaped = advance();
      switch (escaped) {
        case 'n':
          value += '\n';
          break;
        case 't':
          value += '\t';
          break;
        case 'r':
          value += '\r';
          break;
        case '\\':
          value += '\\';
          break;
        case '"':
          value += '"';
          break;
        case '\'':
          value += '\'';
          break;
        default:
          value += escaped;
          break;
      }
    } else if (peek() == '\n') {
      return error_token("Unterminated string");
    } else {
      value += advance();
    }
  }

  if (is_at_end()) {
    return error_token("Unterminated string");
  }

  advance();
  return Token(TokenType::String, value, startLine, startCol);
}

Token Lexer::make_token(TokenType type) {
  return Token(type, std::string(1, input_[pos_ - 1]), line_, column_ - 1);
}

Token Lexer::error_token(const std::string& message) {
  Token t(TokenType::Error, message, line_, column_);
  return t;
}

double Lexer::parse_engineering_multiplier(const std::string& suffix) {
  std::string lower = suffix;
  std::transform(lower.begin(), lower.end(), lower.begin(),
                 [](unsigned char c) { return std::tolower(c); });

  if (lower == "t")
    return 1e12;
  if (lower == "g")
    return 1e9;
  if (lower == "meg")
    return 1e6;
  if (lower == "k")
    return 1e3;
  if (lower == "m")
    return 1e-3;
  if (lower == "u" || lower == "µ")
    return 1e-6;
  if (lower == "n")
    return 1e-9;
  if (lower == "p")
    return 1e-12;
  if (lower == "f")
    return 1e-15;
  if (lower == "a")
    return 1e-18;

  return 1.0;
}

}  // namespace spice_expr
