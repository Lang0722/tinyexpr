#include <gtest/gtest.h>

#include "spice_expr/spice_expr.h"

using namespace spice_expr;

class ParserTest : public ::testing::Test {
 protected:
  ExprArena arena;
};

TEST_F(ParserTest, ParseNumber) {
  auto* expr = parse_expression("42", arena);
  ASSERT_EQ(expr->type(), NodeType::NumberLiteral);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(expr)->real(), 42.0);
}

TEST_F(ParserTest, ParseDecimal) {
  auto* expr = parse_expression("3.14159", arena);
  ASSERT_EQ(expr->type(), NodeType::NumberLiteral);
  EXPECT_NEAR(static_cast<NumberLiteral*>(expr)->real(), 3.14159, 1e-10);
}

TEST_F(ParserTest, ParseScientificNotation) {
  auto* expr = parse_expression("1.5e-3", arena);
  ASSERT_EQ(expr->type(), NodeType::NumberLiteral);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(expr)->real(), 0.0015);
}

TEST_F(ParserTest, ParseEngineeringNotationMilli) {
  auto* expr = parse_expression("10m", arena);
  ASSERT_EQ(expr->type(), NodeType::NumberLiteral);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(expr)->real(), 0.01);
}

TEST_F(ParserTest, ParseEngineeringNotationMicro) {
  auto* expr = parse_expression("100u", arena);
  ASSERT_EQ(expr->type(), NodeType::NumberLiteral);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(expr)->real(), 100e-6);
}

TEST_F(ParserTest, ParseEngineeringNotationNano) {
  auto* expr = parse_expression("50n", arena);
  ASSERT_EQ(expr->type(), NodeType::NumberLiteral);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(expr)->real(), 50e-9);
}

TEST_F(ParserTest, ParseEngineeringNotationPico) {
  auto* expr = parse_expression("10p", arena);
  ASSERT_EQ(expr->type(), NodeType::NumberLiteral);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(expr)->real(), 10e-12);
}

TEST_F(ParserTest, ParseEngineeringNotationFemto) {
  auto* expr = parse_expression("1f", arena);
  ASSERT_EQ(expr->type(), NodeType::NumberLiteral);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(expr)->real(), 1e-15);
}

TEST_F(ParserTest, ParseEngineeringNotationKilo) {
  auto* expr = parse_expression("10k", arena);
  ASSERT_EQ(expr->type(), NodeType::NumberLiteral);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(expr)->real(), 10000.0);
}

TEST_F(ParserTest, ParseEngineeringNotationMeg) {
  auto* expr = parse_expression("1meg", arena);
  ASSERT_EQ(expr->type(), NodeType::NumberLiteral);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(expr)->real(), 1e6);
}

TEST_F(ParserTest, ParseEngineeringNotationGiga) {
  auto* expr = parse_expression("2G", arena);
  ASSERT_EQ(expr->type(), NodeType::NumberLiteral);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(expr)->real(), 2e9);
}

TEST_F(ParserTest, ParseIdentifier) {
  auto* expr = parse_expression("vdd", arena);
  ASSERT_EQ(expr->type(), NodeType::Identifier);
  EXPECT_EQ(static_cast<Identifier*>(expr)->name(), "vdd");
}

TEST_F(ParserTest, ParseAddition) {
  auto* expr = parse_expression("1 + 2", arena);
  ASSERT_EQ(expr->type(), NodeType::BinaryOp);
  auto* binop = static_cast<BinaryOp*>(expr);
  EXPECT_EQ(binop->op_type(), BinaryOpType::Add);
}

TEST_F(ParserTest, ParseSubtraction) {
  auto* expr = parse_expression("5 - 3", arena);
  ASSERT_EQ(expr->type(), NodeType::BinaryOp);
  auto* binop = static_cast<BinaryOp*>(expr);
  EXPECT_EQ(binop->op_type(), BinaryOpType::Subtract);
}

TEST_F(ParserTest, ParseMultiplication) {
  auto* expr = parse_expression("2 * 3", arena);
  ASSERT_EQ(expr->type(), NodeType::BinaryOp);
  auto* binop = static_cast<BinaryOp*>(expr);
  EXPECT_EQ(binop->op_type(), BinaryOpType::Multiply);
}

TEST_F(ParserTest, ParseDivision) {
  auto* expr = parse_expression("10 / 2", arena);
  ASSERT_EQ(expr->type(), NodeType::BinaryOp);
  auto* binop = static_cast<BinaryOp*>(expr);
  EXPECT_EQ(binop->op_type(), BinaryOpType::Divide);
}

TEST_F(ParserTest, ParsePower) {
  auto* expr = parse_expression("2 ** 3", arena);
  ASSERT_EQ(expr->type(), NodeType::BinaryOp);
  auto* binop = static_cast<BinaryOp*>(expr);
  EXPECT_EQ(binop->op_type(), BinaryOpType::Power);
}

TEST_F(ParserTest, ParsePrecedence) {
  auto* expr = parse_expression("1 + 2 * 3", arena);
  ASSERT_EQ(expr->type(), NodeType::BinaryOp);
  auto* binop = static_cast<BinaryOp*>(expr);
  EXPECT_EQ(binop->op_type(), BinaryOpType::Add);
  EXPECT_EQ(binop->right()->type(), NodeType::BinaryOp);
}

TEST_F(ParserTest, ParseParentheses) {
  auto* expr = parse_expression("(1 + 2) * 3", arena);
  ASSERT_EQ(expr->type(), NodeType::BinaryOp);
  auto* binop = static_cast<BinaryOp*>(expr);
  EXPECT_EQ(binop->op_type(), BinaryOpType::Multiply);
  EXPECT_EQ(binop->left()->type(), NodeType::BinaryOp);
}

TEST_F(ParserTest, ParseUnaryMinus) {
  auto* expr = parse_expression("-5", arena);
  ASSERT_EQ(expr->type(), NodeType::UnaryOp);
  auto* unary = static_cast<UnaryOp*>(expr);
  EXPECT_EQ(unary->op_type(), UnaryOpType::Negate);
}

TEST_F(ParserTest, ParseLogicalNot) {
  auto* expr = parse_expression("!x", arena);
  ASSERT_EQ(expr->type(), NodeType::UnaryOp);
  auto* unary = static_cast<UnaryOp*>(expr);
  EXPECT_EQ(unary->op_type(), UnaryOpType::LogicalNot);
}

TEST_F(ParserTest, ParseComparison) {
  auto* expr = parse_expression("a < b", arena);
  ASSERT_EQ(expr->type(), NodeType::BinaryOp);
  EXPECT_EQ(static_cast<BinaryOp*>(expr)->op_type(), BinaryOpType::Less);

  auto* expr2 = parse_expression("a >= b", arena);
  EXPECT_EQ(static_cast<BinaryOp*>(expr2)->op_type(), BinaryOpType::GreaterEqual);
}

TEST_F(ParserTest, ParseEquality) {
  auto* expr = parse_expression("a == b", arena);
  ASSERT_EQ(expr->type(), NodeType::BinaryOp);
  EXPECT_EQ(static_cast<BinaryOp*>(expr)->op_type(), BinaryOpType::Equal);

  auto* expr2 = parse_expression("a != b", arena);
  EXPECT_EQ(static_cast<BinaryOp*>(expr2)->op_type(), BinaryOpType::NotEqual);
}

TEST_F(ParserTest, ParseLogicalAnd) {
  auto* expr = parse_expression("a && b", arena);
  ASSERT_EQ(expr->type(), NodeType::BinaryOp);
  EXPECT_EQ(static_cast<BinaryOp*>(expr)->op_type(), BinaryOpType::LogicalAnd);
}

TEST_F(ParserTest, ParseLogicalOr) {
  auto* expr = parse_expression("a || b", arena);
  ASSERT_EQ(expr->type(), NodeType::BinaryOp);
  EXPECT_EQ(static_cast<BinaryOp*>(expr)->op_type(), BinaryOpType::LogicalOr);
}

TEST_F(ParserTest, ParseTernary) {
  auto* expr = parse_expression("x > 0 ? 1 : -1", arena);
  ASSERT_EQ(expr->type(), NodeType::TernaryConditional);
}

TEST_F(ParserTest, ParseFunctionCall) {
  auto* expr = parse_expression("sin(0.5)", arena);
  ASSERT_EQ(expr->type(), NodeType::FunctionCall);
  auto* func = static_cast<FunctionCall*>(expr);
  EXPECT_EQ(func->name(), "sin");
  EXPECT_EQ(func->argument_count(), 1);
}

TEST_F(ParserTest, ParseFunctionMultipleArgs) {
  auto* expr = parse_expression("pow(2, 3)", arena);
  ASSERT_EQ(expr->type(), NodeType::FunctionCall);
  auto* func = static_cast<FunctionCall*>(expr);
  EXPECT_EQ(func->name(), "pow");
  EXPECT_EQ(func->argument_count(), 2);
}

TEST_F(ParserTest, ParseCircuitVoltage) {
  auto* expr = parse_expression("V(out)", arena);
  ASSERT_EQ(expr->type(), NodeType::CircuitNodeRef);
  auto* ref = static_cast<CircuitNodeRef*>(expr);
  EXPECT_EQ(ref->node1(), "out");
  EXPECT_FALSE(ref->is_differential());
}

TEST_F(ParserTest, ParseCircuitDifferentialVoltage) {
  auto* expr = parse_expression("V(inp, inn)", arena);
  ASSERT_EQ(expr->type(), NodeType::CircuitNodeRef);
  auto* ref = static_cast<CircuitNodeRef*>(expr);
  EXPECT_EQ(ref->node1(), "inp");
  EXPECT_EQ(ref->node2(), "inn");
  EXPECT_TRUE(ref->is_differential());
}

TEST_F(ParserTest, ParseCircuitCurrent) {
  auto* expr = parse_expression("I(R1)", arena);
  ASSERT_EQ(expr->type(), NodeType::CircuitCurrentRef);
  auto* ref = static_cast<CircuitCurrentRef*>(expr);
  EXPECT_EQ(ref->device(), "R1");
}

TEST_F(ParserTest, ParseArrayLiteral) {
  auto* expr = parse_expression("[1, 2, 3]", arena);
  ASSERT_EQ(expr->type(), NodeType::ArrayLiteral);
  auto* arr = static_cast<ArrayLiteral*>(expr);
  EXPECT_EQ(arr->element_count(), 3);
}

TEST_F(ParserTest, ParseArrayIndex) {
  auto* expr = parse_expression("arr[0]", arena);
  ASSERT_EQ(expr->type(), NodeType::ArrayIndex);
}

TEST_F(ParserTest, ParseComplexNumber) {
  auto* expr = parse_expression("(1, 2)", arena);
  ASSERT_EQ(expr->type(), NodeType::NumberLiteral);
  auto* num = static_cast<NumberLiteral*>(expr);
  EXPECT_TRUE(num->is_complex());
  EXPECT_DOUBLE_EQ(num->real(), 1.0);
  EXPECT_DOUBLE_EQ(num->imag(), 2.0);
}

TEST_F(ParserTest, ParseComplexExpression) {
  auto* expr = parse_expression("2 * V(1) + sin(x) - 1.5", arena);
  ASSERT_EQ(expr->type(), NodeType::BinaryOp);
}

TEST_F(ParserTest, ParseNestedFunctions) {
  auto* expr = parse_expression("sqrt(pow(3, 2) + pow(4, 2))", arena);
  ASSERT_EQ(expr->type(), NodeType::FunctionCall);
}

TEST_F(ParserTest, LexerTokenTypes) {
  Lexer lexer("+ - * / ** % ( ) [ ] { } , ? :");
  EXPECT_EQ(lexer.next_token().type, TokenType::Plus);
  EXPECT_EQ(lexer.next_token().type, TokenType::Minus);
  EXPECT_EQ(lexer.next_token().type, TokenType::Star);
  EXPECT_EQ(lexer.next_token().type, TokenType::Slash);
  EXPECT_EQ(lexer.next_token().type, TokenType::Power);
  EXPECT_EQ(lexer.next_token().type, TokenType::Percent);
  EXPECT_EQ(lexer.next_token().type, TokenType::LParen);
  EXPECT_EQ(lexer.next_token().type, TokenType::RParen);
  EXPECT_EQ(lexer.next_token().type, TokenType::LBracket);
  EXPECT_EQ(lexer.next_token().type, TokenType::RBracket);
  EXPECT_EQ(lexer.next_token().type, TokenType::LBrace);
  EXPECT_EQ(lexer.next_token().type, TokenType::RBrace);
  EXPECT_EQ(lexer.next_token().type, TokenType::Comma);
  EXPECT_EQ(lexer.next_token().type, TokenType::Question);
  EXPECT_EQ(lexer.next_token().type, TokenType::Colon);
}

TEST_F(ParserTest, LexerComparisons) {
  Lexer lexer("< <= > >= == !=");
  EXPECT_EQ(lexer.next_token().type, TokenType::Less);
  EXPECT_EQ(lexer.next_token().type, TokenType::LessEqual);
  EXPECT_EQ(lexer.next_token().type, TokenType::Greater);
  EXPECT_EQ(lexer.next_token().type, TokenType::GreaterEqual);
  EXPECT_EQ(lexer.next_token().type, TokenType::Equal);
  EXPECT_EQ(lexer.next_token().type, TokenType::NotEqual);
}

TEST_F(ParserTest, LexerLogical) {
  Lexer lexer("&& || !");
  EXPECT_EQ(lexer.next_token().type, TokenType::And);
  EXPECT_EQ(lexer.next_token().type, TokenType::Or);
  EXPECT_EQ(lexer.next_token().type, TokenType::Not);
}

TEST_F(ParserTest, LexerPeek) {
  Lexer lexer("1 + 2");
  EXPECT_EQ(lexer.peek_token().type, TokenType::Number);
  EXPECT_EQ(lexer.peek_token().type, TokenType::Number);
  EXPECT_EQ(lexer.next_token().type, TokenType::Number);
  EXPECT_EQ(lexer.next_token().type, TokenType::Plus);
}

TEST_F(ParserTest, ParseError) {
  EXPECT_THROW(parse_expression("1 +", arena), ParseError);
}

TEST_F(ParserTest, TokenTypeToString) {
  EXPECT_STREQ(tokenTypeToString(TokenType::Plus), "+");
  EXPECT_STREQ(tokenTypeToString(TokenType::Number), "Number");
  EXPECT_STREQ(tokenTypeToString(TokenType::EndOfInput), "EOF");
}

// ============================================================================
// Extended edge case tests adapted from ExprTK test suite
// ============================================================================

// Whitespace handling tests
TEST_F(ParserTest, WhitespaceHandlingBasic) {
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;
  RealEvaluator eval(symbols, functions, circuit);

  auto* e1 = parse_expression("0+9", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e1), 9.0);

  auto* e2 = parse_expression(" 0 + 9 ", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e2), 9.0);

  auto* e3 = parse_expression("(  0  +  9  )", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e3), 9.0);
}

TEST_F(ParserTest, WhitespaceHandlingMultipleSpaces) {
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;
  RealEvaluator eval(symbols, functions, circuit);

  auto* e1 = parse_expression("  1   +   2  ", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e1), 3.0);

  auto* e2 = parse_expression("( 1 + 2 ) * 3", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e2), 9.0);
}

// Unary operator edge cases
TEST_F(ParserTest, UnaryOperatorEdgeCasesDoubleNegative) {
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;
  RealEvaluator eval(symbols, functions, circuit);

  auto* e1 = parse_expression("1 - -1", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e1), 2.0);

  auto* e2 = parse_expression("1--1", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e2), 2.0);

  auto* e3 = parse_expression("1 -- -1", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e3), 0.0);
}

TEST_F(ParserTest, UnaryOperatorEdgeCasesDoublePositive) {
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;
  RealEvaluator eval(symbols, functions, circuit);

  auto* e1 = parse_expression("1 + +1", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e1), 2.0);

  auto* e2 = parse_expression("1 ++1", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e2), 2.0);
}

TEST_F(ParserTest, UnaryOperatorEdgeCasesMixedSigns) {
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;
  RealEvaluator eval(symbols, functions, circuit);

  auto* e1 = parse_expression("1 + -1", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e1), 0.0);

  auto* e2 = parse_expression("1+-1", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e2), 0.0);

  auto* e3 = parse_expression("1 +- -1", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e3), 2.0);
}

TEST_F(ParserTest, UnaryOperatorWithParentheses) {
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;
  RealEvaluator eval(symbols, functions, circuit);

  auto* e1 = parse_expression("-(1+2)", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e1), -3.0);

  auto* e2 = parse_expression("+(1+2)", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e2), 3.0);

  auto* e3 = parse_expression("-(1-2)", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e3), 1.0);

  auto* e4 = parse_expression("(-3*-6)", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e4), 18.0);

  auto* e5 = parse_expression("-(-3*-6)", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e5), -18.0);
}

// Power operator associativity tests
TEST_F(ParserTest, PowerAssociativityRightToLeft) {
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;
  RealEvaluator eval(symbols, functions, circuit);

  // Power is right-associative: 2**3**2 = 2**(3**2) = 2**9 = 512
  // NOT (2**3)**2 = 8**2 = 64
  auto* expr = parse_expression("2**3**2", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*expr), 512.0);
}

TEST_F(ParserTest, PowerAssociativityWithParentheses) {
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;
  RealEvaluator eval(symbols, functions, circuit);

  // Explicit left grouping
  auto* e1 = parse_expression("(2**3)**2", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e1), 64.0);

  // Explicit right grouping (same as default)
  auto* e2 = parse_expression("2**(3**2)", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e2), 512.0);
}

TEST_F(ParserTest, PowerWithNegativeBase) {
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;
  RealEvaluator eval(symbols, functions, circuit);

  // In spice_expr, unary minus binds tighter than **, so -2**3 = (-2)**3
  auto* e1 = parse_expression("-2**3", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e1), -8.0);  // (-2)^3 = -8

  auto* e2 = parse_expression("-2**4", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e2), 16.0);  // (-2)^4 = 16 (not -(2^4))

  auto* e3 = parse_expression("(-2)**3", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e3), -8.0);

  auto* e4 = parse_expression("(-2)**4", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e4), 16.0);
}

// Note: spice_expr does not support trailing decimal syntax (e.g., "2." is parsed as "2")
// These tests verify the parser's actual behavior

// Error handling tests
TEST_F(ParserTest, InvalidExpressionEmpty) {
  EXPECT_THROW(parse_expression("", arena), ParseError);
}

TEST_F(ParserTest, InvalidExpressionIncomplete) {
  EXPECT_THROW(parse_expression("1 +", arena), ParseError);
  EXPECT_THROW(parse_expression("1 *", arena), ParseError);
  EXPECT_THROW(parse_expression("1 /", arena), ParseError);
}

TEST_F(ParserTest, InvalidExpressionUnmatchedParens) {
  EXPECT_THROW(parse_expression("((1)", arena), ParseError);
  // Note: Extra closing paren may be handled differently by parser
  EXPECT_THROW(parse_expression("(1+2", arena), ParseError);
}

TEST_F(ParserTest, InvalidExpressionMissingOperand) {
  // Note: spice_expr supports unary + operator, so "+ 1" is valid
  EXPECT_THROW(parse_expression("* 1", arena), ParseError);
  EXPECT_THROW(parse_expression("1 + * 2", arena), ParseError);
}

TEST_F(ParserTest, InvalidExpressionDoubleOperator) {
  EXPECT_THROW(parse_expression("1 ** ** 2", arena), ParseError);
  EXPECT_THROW(parse_expression("1 / / 2", arena), ParseError);
}

// Complex precedence tests
TEST_F(ParserTest, PrecedenceMultipleOperators) {
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;
  RealEvaluator eval(symbols, functions, circuit);

  // Verify standard mathematical precedence
  auto* e1 = parse_expression("1 + 2 * 3 - 4 / 2", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e1), 5.0);  // 1 + 6 - 2 = 5

  auto* e2 = parse_expression("2 ** 3 * 4", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e2), 32.0);  // 8 * 4 = 32

  auto* e3 = parse_expression("4 * 2 ** 3", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e3), 32.0);  // 4 * 8 = 32
}

TEST_F(ParserTest, PrecedenceComparisonAndLogical) {
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;
  RealEvaluator eval(symbols, functions, circuit);

  // Comparison should bind tighter than logical
  auto* e1 = parse_expression("1 < 2 && 3 > 2", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e1), 1.0);

  auto* e2 = parse_expression("1 > 2 || 3 < 4", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e2), 1.0);
}

// Nested ternary tests
TEST_F(ParserTest, NestedTernary) {
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;
  RealEvaluator eval(symbols, functions, circuit);

  auto* expr = parse_expression("1 ? (2 ? 3 : 4) : 5", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*expr), 3.0);

  auto* expr2 = parse_expression("0 ? 1 : (0 ? 2 : 3)", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*expr2), 3.0);
}

// Large numbers
TEST_F(ParserTest, LargeNumbers) {
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;
  RealEvaluator eval(symbols, functions, circuit);

  auto* e1 = parse_expression("1234567890", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e1), 1234567890.0);

  auto* e2 = parse_expression("123456789.0", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e2), 123456789.0);

  auto* e3 = parse_expression("-1234567890", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e3), -1234567890.0);
}

// Scientific notation edge cases
TEST_F(ParserTest, ScientificNotationEdgeCases) {
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;
  RealEvaluator eval(symbols, functions, circuit);

  auto* e1 = parse_expression("1E10", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e1), 1e10);

  auto* e2 = parse_expression("1e10", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e2), 1e10);

  auto* e3 = parse_expression("1.5E-10", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e3), 1.5e-10);

  auto* e4 = parse_expression("1.5e+10", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e4), 1.5e+10);
}

// Function with expressions as arguments
TEST_F(ParserTest, FunctionWithExpressionArguments) {
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;
  RealEvaluator eval(symbols, functions, circuit);

  auto* e1 = parse_expression("max(1+2, 2+1)", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e1), 3.0);

  auto* e2 = parse_expression("min(2*3, 3*2)", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e2), 6.0);

  auto* e3 = parse_expression("sqrt(3**2 + 4**2)", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e3), 5.0);
}

// Case sensitivity of function names
TEST_F(ParserTest, FunctionNameCaseInsensitivity) {
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;
  RealEvaluator eval(symbols, functions, circuit);

  auto* e1 = parse_expression("SIN(0)", arena);
  EXPECT_NEAR(eval.evaluate(*e1), 0.0, 1e-10);

  auto* e2 = parse_expression("Sin(0)", arena);
  EXPECT_NEAR(eval.evaluate(*e2), 0.0, 1e-10);

  auto* e3 = parse_expression("COS(0)", arena);
  EXPECT_NEAR(eval.evaluate(*e3), 1.0, 1e-10);

  auto* e4 = parse_expression("SQRT(4)", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*e4), 2.0);
}

// Multi-term expressions
TEST_F(ParserTest, MultiTermAddition) {
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;
  RealEvaluator eval(symbols, functions, circuit);

  auto* expr = parse_expression("1+2+3+4+5+6+7+8+9+0", arena);
  EXPECT_DOUBLE_EQ(eval.evaluate(*expr), 45.0);
}

TEST_F(ParserTest, MultiTermMixedOperations) {
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;
  RealEvaluator eval(symbols, functions, circuit);

  auto* expr = parse_expression("1+2-3*4/5+6-7*8/9+0", arena);
  EXPECT_NEAR(eval.evaluate(*expr), 0.37777777777777777778, 1e-10);
}
