#include <gtest/gtest.h>

#include <cmath>
#include <string>
#include <vector>

#include "spice_expr/spice_expr.h"

using namespace spice_expr;

// Test expression corpus adapted from ExprTK's global_test_list
// Syntax adaptations:
// - x^y -> x**y (power operator)
// - Tests using ExprTK-specific features (equal(), switch, repeat, etc.) are omitted
// - Using && and || instead of "and" and "or"

struct TestExpr {
  std::string expr;
  double expected;
};

class ExpressionCorpusTest : public ::testing::TestWithParam<TestExpr> {
 protected:
  ExprArena arena;
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;

  double evaluate(const std::string& expr_str) {
    ExprNode* expr = parse_expression(expr_str, arena);
    RealEvaluator eval(symbols, functions, circuit);
    return eval.evaluate(*expr);
  }
};

TEST_P(ExpressionCorpusTest, EvaluatesCorrectly) {
  const auto& test = GetParam();
  double result = evaluate(test.expr);
  if (std::abs(test.expected) < 1e-10) {
    EXPECT_NEAR(result, test.expected, 1e-10) << "Expr: " << test.expr;
  } else {
    EXPECT_NEAR(result, test.expected, std::abs(test.expected) * 1e-10) << "Expr: " << test.expr;
  }
}

// Numeric literals
static const std::vector<TestExpr> numeric_literal_tests = {
    {"0", 0.0},
    {"1", 1.0},
    {"2", 2.0},
    {"3", 3.0},
    {"4", 4.0},
    {"5", 5.0},
    {"6", 6.0},
    {"7", 7.0},
    {"8", 8.0},
    {"9", 9.0},
    {"12.12", 12.12},
    {"123.123", 123.123},
    {"1234.1234", 1234.1234},
    {"12345.12345", 12345.12345},
    {"123456.123456", 123456.123456},
    {"0.0", 0.0},
    {"1.0", 1.0},
    {"2.0", 2.0},
    {"3.0", 3.0},
    {"4.0", 4.0},
    {"5.0", 5.0},
    {"6.0", 6.0},
    {"7.0", 7.0},
    {"8.0", 8.0},
    {"9.0", 9.0},
    {"1.1", 1.1},
    {"2.2", 2.2},
    {"3.3", 3.3},
    {"4.4", 4.4},
    {"5.5", 5.5},
    {"6.6", 6.6},
    {"7.7", 7.7},
    {"8.8", 8.8},
    {"9.9", 9.9},
};

// Signed numbers
static const std::vector<TestExpr> signed_number_tests = {
    {"+0", 0.0},
    {"+1", 1.0},
    {"+2", 2.0},
    {"+3", 3.0},
    {"+4", 4.0},
    {"+5", 5.0},
    {"+6", 6.0},
    {"+7", 7.0},
    {"+8", 8.0},
    {"+9", 9.0},
    {"+0.0", 0.0},
    {"+1.0", 1.0},
    {"+1.1", 1.1},
    {"+2.2", 2.2},
    {"-0", -0.0},
    {"-1", -1.0},
    {"-2", -2.0},
    {"-3", -3.0},
    {"-4", -4.0},
    {"-5", -5.0},
    {"-6", -6.0},
    {"-7", -7.0},
    {"-8", -8.0},
    {"-9", -9.0},
    {"-0.0", -0.0},
    {"-1.0", -1.0},
    {"-1.1", -1.1},
    {"-2.2", -2.2},
    {"-3.3", -3.3},
};

// Scientific notation
static const std::vector<TestExpr> scientific_notation_tests = {
    {"0.0e+0", +0.0e+0},
    {"1.1e+1", +1.1e+1},
    {"2.2e+2", +2.2e+2},
    {"3.3e+3", +3.3e+3},
    {"4.4e+4", +4.4e+4},
    {"5.5e+5", +5.5e+5},
    {"6.6e+6", +6.6e+6},
    {"7.7e+7", +7.7e+7},
    {"8.8e+8", +8.8e+8},
    {"9.9e+9", +9.9e+9},
    {"-0.0e+0", -0.0e+0},
    {"-1.1e+1", -1.1e+1},
    {"-2.2e+2", -2.2e+2},
    {"-3.3e+3", -3.3e+3},
    {"-4.4e+4", -4.4e+4},
    {"0.0E+0", +0.0E+0},
    {"1.1E+1", +1.1E+1},
    {"2.2E+2", +2.2E+2},
    {"3.3E+3", +3.3E+3},
    {"-0.0E+0", -0.0E+0},
    {"-1.1E+1", -1.1E+1},
    {"-2.2E+2", -2.2E+2},
    {"1E1+1", 11.0},
    {"1e1+1", 11.0},
    {"1E1-1", 9.0},
    {"1e1-1", 9.0},
    {"1E01+1", 11.0},
    {"1e01+1", 11.0},
};

// Parenthesized expressions
static const std::vector<TestExpr> parenthesized_tests = {
    {"(0)", 0.0},
    {"(1)", 1.0},
    {"(2)", 2.0},
    {"(3)", 3.0},
    {"(4)", 4.0},
    {"(5)", 5.0},
    {"(6)", 6.0},
    {"(7)", 7.0},
    {"(8)", 8.0},
    {"(9)", 9.0},
    {"(0.0)", 0.0},
    {"(1.0)", 1.0},
    {"(2.0)", 2.0},
    {"(1.1)", 1.1},
    {"(2.2)", 2.2},
    {"(+0)", 0.0},
    {"(+1)", 1.0},
    {"(+2)", 2.0},
    {"(+1.0)", 1.0},
    {"(+1.1)", 1.1},
    {"(-0)", -0.0},
    {"(-1)", -1.0},
    {"(-2)", -2.0},
    {"(-1.0)", -1.0},
    {"(-1.1)", -1.1},
    {"-(1.1)", -1.1},
    {"-(1.1+2.2)", -3.3},
};

// Basic arithmetic
static const std::vector<TestExpr> basic_arithmetic_tests = {
    {"0+9", 9.0},
    {"1+8", 9.0},
    {"2+7", 9.0},
    {"3+6", 9.0},
    {"4+5", 9.0},
    {"5+4", 9.0},
    {"6+3", 9.0},
    {"7+2", 9.0},
    {"8+1", 9.0},
    {"9+0", 9.0},
    {" 0 + 9 ", 9.0},
    {" 1 + 8 ", 9.0},
    {" 2 + 7 ", 9.0},
    {" 3 + 6 ", 9.0},
    {" 4 + 5 ", 9.0},
    {"( 0 + 9 )", 9.0},
    {"( 1 + 8 )", 9.0},
    {"( 2 + 7 )", 9.0},
    {"( 3 + 6 )", 9.0},
    {"( 4 + 5 )", 9.0},
    {"1+2", +3.0},
    {"1-2", -1.0},
    {"1*2", +2.0},
    {"1/2", +0.5},
    {"1.1+2.2", +3.3},
    {"1.1-2.2", -1.1},
    {"1.1*2.2", +2.42},
    {"1.1/2.2", +0.5},
    {"0-9", -9.0},
    {"1-8", -7.0},
    {"2-7", -5.0},
    {"3-6", -3.0},
    {"4-5", -1.0},
    {"5-4", +1.0},
    {"6-3", +3.0},
    {"7-2", +5.0},
    {"8-1", +7.0},
    {"9-0", +9.0},
};

// Unary operator edge cases
static const std::vector<TestExpr> unary_operator_tests = {
    {"1 - -1", 2.0},
    {"1 --1", 2.0},
    {"1-- 1", 2.0},
    {"1--1", 2.0},
    {"1 -- -1", 0.0},
    {"1 + -1", 0.0},
    {"1 +-1", 0.0},
    {"1+- 1", 0.0},
    {"1+-1", 0.0},
    {"1 +- -1", 2.0},
    {"1 + +1", 2.0},
    {"1 ++1", 2.0},
    {"1 - -1 + 1", 3.0},
    {"1 --1 + 1", 3.0},
    {"1-- 1 + 1", 3.0},
    {"1--1 + 1", 3.0},
    {"1 -- -1 + 1", 1.0},
    {"1 + -1 + 1", 1.0},
    {"1 +-1 + 1", 1.0},
    {"1+- 1 + 1", 1.0},
    {"1+-1 + 1", 1.0},
    {"1 +- -1 + 1", 3.0},
    {"1 + +1 + 1", 3.0},
    {"1 ++1 + 1", 3.0},
    {"1 - -1 - 1", 1.0},
    {"1 --1 - 1", 1.0},
    {"1-- 1 - 1", 1.0},
    {"1--1 - 1", 1.0},
    {"1 -- -1 - 1", -1.0},
    {"1 + -1 - 1", -1.0},
    {"1 +-1 - 1", -1.0},
    {"1+- 1 - 1", -1.0},
    {"1+-1 - 1", -1.0},
    {"1 +- -1 - 1", 1.0},
    {"1 + +1 - 1", 1.0},
    {"1 ++1 - 1", 1.0},
    {"-(1+2)", -3.0},
    {"+(1+2)", +3.0},
    {"+(1-2)", -1.0},
    {"-(1-2)", +1.0},
    {"(-3*-6)", +18.0},
    {"(-6*-3)", +18.0},
    {"-(-3*-6)", -18.0},
    {"-(-6*-3)", -18.0},
};

// Multi-term arithmetic
static const std::vector<TestExpr> multi_term_tests = {
    {"1.1+2.2+3.3", +6.6},
    {"+1.1+2.2+3.3", +6.6},
    {"-1.1-2.2-3.3", -6.6},
    {"1.1*2.2*3.3", +7.986},
    {"+1.1*2.2*3.3", +7.986},
    {"-1.1*-2.2*-3.3", -7.986},
    {"1 + 1/2", +1.5},
    {"1 + (1/2)", +1.5},
    {"1.1 + 1.1/2.2", +1.6},
    {"1.1 + (1.1/2.2)", +1.6},
    {"2 * 1/2", +1.0},
    {"2 * (1/2)", +1.0},
    {"2.2 * 1.1/2.2", +1.1},
    {"2.2 * (1.1/2.2)", +1.1},
};

// Power operator tests (using **)
// Note: In spice_expr, unary minus binds tighter than **, so -2**4 = (-2)**4 = 16
static const std::vector<TestExpr> power_tests = {
    {"1**2", 1.0},
    {"2**1", 2.0},
    {"2**3", 8.0},
    {"-2**3", -8.0},   // (-2)^3 = -8
    {"-2**4", 16.0},   // (-2)^4 = 16 (unary minus binds tighter)
    {"(-2)**3", -8.0},
    {"(-2)**4", +16.0},
    {"3**2**4", 43046721.0},  // Right associative: 3^(2^4) = 3^16
    {"1.1**2.2", 1.23328630055466251099},
    {"2.2**1.1", 2.3804822576003541627},
    {"2.2**3.3", 13.48946876053338489127},
    {"3.3**2.2**1.1", 17.15193942371376191362},
    {"+3.3**2.2**1.1", 17.15193942371376191362},
    {"3.3**+2.2**1.1", 17.15193942371376191362},
    {"3.3**2.2**+1.1", 17.15193942371376191362},
    {"3.3**2.2**-1.1", 1.65127293793867959137},
    {"+3.3**+2.2**-1.1", 1.65127293793867959137},
    {"1.1**(1.1 * 2.2)", 1.25941916576299080582},
    {"2.2**(1.1 * 3.3)", 17.49823848953534759743},
    {"3.3**(1.1 * 2.2)", 17.98058156638874965269},
};

// Comparison operators
static const std::vector<TestExpr> comparison_tests = {
    {"1 < 2", 1.0},
    {"1 <= 2", 1.0},
    {"1.1 <= 2.2", 1.0},
    {"(1.0 + 0.1) <= (2.0 + 0.2)", 1.0},
    {"1 > 2", 0.0},
    {"1 >= 2", 0.0},
    {"1.1 >= 2.2", 0.0},
    {"(1.0 + 0.1) >= (2.0 + 0.2)", 0.0},
    {"1 != 2", 1.0},
    {"1.1 != 2.2", 1.0},
    {"(1.0 + 0.1) != (2.0 + 0.2)", 1.0},
    {"1 == 1", 1.0},
    {"1.1 == 1.1", 1.0},
    {"1 != 1", 0.0},
    {"1.1 != 1.1", 0.0},
    {"(1.0 + 0.1) != (1.0 + 0.1)", 0.0},
};

// Logical operators
static const std::vector<TestExpr> logical_tests = {
    {"1 && 1", 1.0},
    {"1 && 0", 0.0},
    {"0 && 1", 0.0},
    {"0 && 0", 0.0},
    {"1.0 && 1.0", 1.0},
    {"1.0 && 0.0", 0.0},
    {"0.0 && 1.0", 0.0},
    {"0.0 && 0.0", 0.0},
    {"(1 && 1)", 1.0},
    {"(1 && 0)", 0.0},
    {"(0 && 1)", 0.0},
    {"(0 && 0)", 0.0},
    {"1 || 1", 1.0},
    {"1 || 0", 1.0},
    {"0 || 1", 1.0},
    {"0 || 0", 0.0},
    {"1.0 || 1.0", 1.0},
    {"1.0 || 0.0", 1.0},
    {"0.0 || 1.0", 1.0},
    {"0.0 || 0.0", 0.0},
    {"(1 || 1)", 1.0},
    {"(1 || 0)", 1.0},
    {"(0 || 1)", 1.0},
    {"(0 || 0)", 0.0},
};

// Built-in functions
static const std::vector<TestExpr> function_tests = {
    {"abs(1)", 1.0},
    {"abs(-1)", 1.0},
    {"abs(1.0)", 1.0},
    {"abs(-1.0)", 1.0},
    {"min(1,2)", 1.0},
    {"min(1,2,3)", 1.0},
    {"min(1,2,3,4)", 1.0},
    {"min(1.1,2.2)", 1.1},
    {"min(1.1,2.2,3.3)", 1.1},
    {"min(min(1,2),min(3,4))", 1.0},
    {"max(1,2)", 2.0},
    {"max(1,2,3)", 3.0},
    {"max(1,2,3,4)", 4.0},
    {"max(1.1,2.2)", 2.2},
    {"max(1.1,2.2,3.3)", 3.3},
    {"max(max(1,2),max(3,4))", 4.0},
    {"floor(1.0)", 1.0},
    {"floor(1.1)", 1.0},
    {"floor(-1.0)", -1.0},
    {"floor(-1.1)", -2.0},
    {"ceil(1.0)", 1.0},
    {"ceil(1.1)", 2.0},
    {"ceil(-1.0)", -1.0},
    {"ceil(-1.1)", -1.0},
    {"cos(0.0)", 1.0},
    {"sin(0.0)", 0.0},
    {"exp(1.0)", 2.71828182845904523536028747135266249775724},
    {"exp(0.0)", 1.0},
    {"log(2.7182818284590451)", 1.0},
    {"log10(10.0)", 1.0},
    {"sqrt(4.0)", 2.0},
    {"sqrt(9.0)", 3.0},
    {"sqrt(16.0)", 4.0},
};

// Ternary conditional
static const std::vector<TestExpr> ternary_tests = {
    {"0 ? 1 : 2", 2.0},
    {"1 ? 3 : 4", 3.0},
    {"(0 ? 1 : 2) == 2", 1.0},
    {"(1 ? 3 : 4) == 3", 1.0},
    {"(1 < 2 ? 3 : 4) == 3", 1.0},
    {"(1 > 2 ? 3 : 4) == 4", 1.0},
    {"(1 < 2 ? 3 + 5 : 4) == 8", 1.0},
    {"(1 > 2 ? 3 : 4 + 5) == 9", 1.0},
    {"(2 < 3 + 3 ? 7 : 9) == 7", 1.0},
    {"(1 + 1 < 3 ? 7 : 9) == 7", 1.0},
    {"(1 + 1 < 3 + 3 ? 7 : 9) == 7", 1.0},
    {"(2 > 3 + 3 ? 7 : 9) == 9", 1.0},
    {"(1 + 1 > 3 ? 7 : 9) == 9", 1.0},
    {"(1 + 1 > 3 + 3 ? 7 : 9) == 9", 1.0},
};

// Complex multi-term expressions
static const std::vector<TestExpr> complex_expression_tests = {
    {"1+2+3+4+5+6+7+8+9+0", 45.0},
    {"1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 0", 45.0},
    {"1.0 + 2.0 + 3.0 + 4.0 + 5.0 + 6.0 + 7.0 + 8.0 + 9.0 + 0.0", 45.0},
    {"(1+2)+(3+4)+(5+6)+(7+8)+(9+0)", 45.0},
    {"(1-2)+(3-4)+(5-6)+(7-8)+(9-0)", +5.0},
    {"(1+2)-(3+4)-(5+6)-(7+8)-(9+0)", -39.0},
    {"(1.0+2.0)+(3.0+4.0)+(5.0+6.0)+(7.0+8.0)+(9.0+0.0)", 45.0},
    {"(1.0-2.0)+(3.0-4.0)+(5.0-6.0)+(7.0-8.0)+(9.0-0.0)", +5.0},
    {"(1.0+2.0)-(3.0+4.0)-(5.0+6.0)-(7.0+8.0)-(9.0+0.0)", -39.0},
    {"1+2-3*4/5+6-7*8/9+0", 0.37777777777777777778},
    {"1.1+2.2-3.3*4.4/5.5+6.6-7.7*8.8/9.9+0.0", 0.41555555555555555556},
    {"(1+2)-(3*4)/(5+6)-(7*8)/(9+0)", -4.31313131313131313131},
    {"1/1+1/2+1/3+1/4+1/5+1/6+1/7+1/8+1/9", 2.82896825396825396825},
    {"(1/1)+(1/2)+(1/3)+(1/4)+(1/5)+(1/6)+(1/7)+(1/8)+(1/9)", 2.82896825396825396825},
    {"1/1*1/2*1/3*1/4*1/5*1/6*1/7*1/8*1/9", 0.00000275573192239859},
    {"(1/1)*(1/2)*(1/3)*(1/4)*(1/5)*(1/6)*(1/7)*(1/8)*(1/9)", 0.00000275573192239859},
};

// Limit function tests (adapted from clamp)
static const std::vector<TestExpr> limit_function_tests = {
    {"limit(1, -1, +1)", 1.0},
    {"limit(-1.5, -1, +1.0)", -1.0},
    {"limit(+1.5, -1, +1.0)", +1.0},
    {"limit(-1.5, -1, +1.0) + limit(+1.5, -1, +1.0)", 0.0},
};

INSTANTIATE_TEST_SUITE_P(NumericLiterals, ExpressionCorpusTest,
                         ::testing::ValuesIn(numeric_literal_tests));

INSTANTIATE_TEST_SUITE_P(SignedNumbers, ExpressionCorpusTest,
                         ::testing::ValuesIn(signed_number_tests));

INSTANTIATE_TEST_SUITE_P(ScientificNotation, ExpressionCorpusTest,
                         ::testing::ValuesIn(scientific_notation_tests));

INSTANTIATE_TEST_SUITE_P(Parenthesized, ExpressionCorpusTest,
                         ::testing::ValuesIn(parenthesized_tests));

INSTANTIATE_TEST_SUITE_P(BasicArithmetic, ExpressionCorpusTest,
                         ::testing::ValuesIn(basic_arithmetic_tests));

INSTANTIATE_TEST_SUITE_P(UnaryOperators, ExpressionCorpusTest,
                         ::testing::ValuesIn(unary_operator_tests));

INSTANTIATE_TEST_SUITE_P(MultiTerm, ExpressionCorpusTest, ::testing::ValuesIn(multi_term_tests));

INSTANTIATE_TEST_SUITE_P(Power, ExpressionCorpusTest, ::testing::ValuesIn(power_tests));

INSTANTIATE_TEST_SUITE_P(Comparison, ExpressionCorpusTest, ::testing::ValuesIn(comparison_tests));

INSTANTIATE_TEST_SUITE_P(Logical, ExpressionCorpusTest, ::testing::ValuesIn(logical_tests));

INSTANTIATE_TEST_SUITE_P(Functions, ExpressionCorpusTest, ::testing::ValuesIn(function_tests));

INSTANTIATE_TEST_SUITE_P(Ternary, ExpressionCorpusTest, ::testing::ValuesIn(ternary_tests));

INSTANTIATE_TEST_SUITE_P(ComplexExpressions, ExpressionCorpusTest,
                         ::testing::ValuesIn(complex_expression_tests));

INSTANTIATE_TEST_SUITE_P(LimitFunction, ExpressionCorpusTest,
                         ::testing::ValuesIn(limit_function_tests));
