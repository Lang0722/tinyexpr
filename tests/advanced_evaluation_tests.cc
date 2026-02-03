// Advanced evaluation tests adapted from ExprTK test patterns
// Tests variable-based expressions, user-defined functions, and complex arithmetic

#include <gtest/gtest.h>

#include <cmath>
#include <string>
#include <vector>

#include "spice_expr/spice_expr.h"

using namespace spice_expr;

// =============================================================================
// Test fixture with x, y variables (similar to ExprTK test_xy)
// =============================================================================

class VariableExpressionTest : public ::testing::Test {
 protected:
  ExprArena arena;
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;

  void SetUp() override {
    // Default values, can be overridden per test
    symbols.define("x", arena.make<NumberLiteral>(0.0));
    symbols.define("y", arena.make<NumberLiteral>(0.0));
  }

  void set_xy(double x_val, double y_val) {
    symbols.set_override("x", arena.make<NumberLiteral>(x_val));
    symbols.set_override("y", arena.make<NumberLiteral>(y_val));
  }

  double evaluate(const std::string& expr_str) {
    ExprNode* expr = parse_expression(expr_str, arena);
    RealEvaluator eval(symbols, functions, circuit);
    return eval.evaluate(*expr);
  }

  bool near_equal(double a, double b, double epsilon = 1e-10) {
    if (std::isnan(a) || std::isnan(b)) return false;
    double diff = std::abs(a - b);
    double norm = std::max(1.0, std::max(std::abs(a), std::abs(b)));
    return diff <= epsilon * norm;
  }
};

// Basic arithmetic with variables
TEST_F(VariableExpressionTest, BasicArithmetic) {
  set_xy(2.2, 3.3);

  EXPECT_NEAR(evaluate("x + y"), 5.5, 1e-10);
  EXPECT_NEAR(evaluate("x - y"), -1.1, 1e-10);
  EXPECT_NEAR(evaluate("x * y"), 7.26, 1e-10);
  EXPECT_NEAR(evaluate("x / y"), 2.2 / 3.3, 1e-10);
}

TEST_F(VariableExpressionTest, CompoundExpressions) {
  set_xy(2.2, 3.3);

  EXPECT_NEAR(evaluate("(x + y) * (x + y)"), 30.25, 1e-10);
  EXPECT_NEAR(evaluate("(x + y) / (x + y)"), 1.0, 1e-10);
  EXPECT_NEAR(evaluate("1 + (x + y)"), 6.5, 1e-10);
  EXPECT_NEAR(evaluate("(x + y) - 1"), 4.5, 1e-10);
  EXPECT_NEAR(evaluate("1 + (x + y) * 2"), 12.0, 1e-10);
  EXPECT_NEAR(evaluate("2 * (x + y) - 1"), 10.0, 1e-10);
}

TEST_F(VariableExpressionTest, UnaryOperatorsWithVariables) {
  set_xy(1.0, 0.0);

  EXPECT_NEAR(evaluate("x - -1"), 2.0, 1e-10);
  EXPECT_NEAR(evaluate("x --1"), 2.0, 1e-10);
  EXPECT_NEAR(evaluate("x-- 1"), 2.0, 1e-10);
  EXPECT_NEAR(evaluate("x--1"), 2.0, 1e-10);
  EXPECT_NEAR(evaluate("x -- -1"), 0.0, 1e-10);
  EXPECT_NEAR(evaluate("x + -1"), 0.0, 1e-10);
  EXPECT_NEAR(evaluate("x +-1"), 0.0, 1e-10);
  EXPECT_NEAR(evaluate("x+- 1"), 0.0, 1e-10);
  EXPECT_NEAR(evaluate("x+-1"), 0.0, 1e-10);
  EXPECT_NEAR(evaluate("x +- -1"), 2.0, 1e-10);
  EXPECT_NEAR(evaluate("x + +1"), 2.0, 1e-10);
  EXPECT_NEAR(evaluate("x ++1"), 2.0, 1e-10);
}

TEST_F(VariableExpressionTest, UnaryWithParentheses) {
  set_xy(1.0, 0.0);

  EXPECT_NEAR(evaluate("(x - -1 + 1)"), 3.0, 1e-10);
  EXPECT_NEAR(evaluate("(x --1 + 1)"), 3.0, 1e-10);
  EXPECT_NEAR(evaluate("(x-- 1 + 1)"), 3.0, 1e-10);
  EXPECT_NEAR(evaluate("(x--1 + 1)"), 3.0, 1e-10);
  EXPECT_NEAR(evaluate("(x -- -1 + 1)"), 1.0, 1e-10);
  EXPECT_NEAR(evaluate("(x + -1 + 1)"), 1.0, 1e-10);
  EXPECT_NEAR(evaluate("(x +-1 + 1)"), 1.0, 1e-10);
  EXPECT_NEAR(evaluate("(x+- 1 + 1)"), 1.0, 1e-10);
  EXPECT_NEAR(evaluate("(x+-1 + 1)"), 1.0, 1e-10);
  EXPECT_NEAR(evaluate("(x +- -1 + 1)"), 3.0, 1e-10);
  EXPECT_NEAR(evaluate("(x + +1 + 1)"), 3.0, 1e-10);
  EXPECT_NEAR(evaluate("(x ++1 + 1)"), 3.0, 1e-10);
}

TEST_F(VariableExpressionTest, IdentityOperations) {
  set_xy(2.0, 3.0);

  EXPECT_NEAR(evaluate("x * 1"), 2.0, 1e-10);
  EXPECT_NEAR(evaluate("1 * x"), 2.0, 1e-10);
  EXPECT_NEAR(evaluate("y * 1"), 3.0, 1e-10);
  EXPECT_NEAR(evaluate("1 * y"), 3.0, 1e-10);
  EXPECT_NEAR(evaluate("x * 0"), 0.0, 1e-10);
  EXPECT_NEAR(evaluate("0 * x"), 0.0, 1e-10);
  EXPECT_NEAR(evaluate("y * 0"), 0.0, 1e-10);
  EXPECT_NEAR(evaluate("0 * y"), 0.0, 1e-10);
}

TEST_F(VariableExpressionTest, CommutativeOperations) {
  set_xy(2.0, 3.0);

  EXPECT_NEAR(evaluate("x + 1"), evaluate("1 + x"), 1e-10);
  EXPECT_NEAR(evaluate("y + 1"), evaluate("1 + y"), 1e-10);
  EXPECT_NEAR(evaluate("x + y"), evaluate("y + x"), 1e-10);
  EXPECT_NEAR(evaluate("x * y"), evaluate("y * x"), 1e-10);
}

TEST_F(VariableExpressionTest, Comparisons) {
  set_xy(2.0, 3.0);

  EXPECT_DOUBLE_EQ(evaluate("x < y"), 1.0);
  EXPECT_DOUBLE_EQ(evaluate("y > x"), 1.0);
  EXPECT_DOUBLE_EQ(evaluate("x <= y"), 1.0);
  EXPECT_DOUBLE_EQ(evaluate("y >= x"), 1.0);
  EXPECT_DOUBLE_EQ(evaluate("x + y > y"), 1.0);
  EXPECT_DOUBLE_EQ(evaluate("x + y > x"), 1.0);
  EXPECT_DOUBLE_EQ(evaluate("x * y > y"), 1.0);
  EXPECT_DOUBLE_EQ(evaluate("x * y > x"), 1.0);
  EXPECT_DOUBLE_EQ(evaluate("(x + y) > y"), 1.0);
  EXPECT_DOUBLE_EQ(evaluate("(x + y) > x"), 1.0);
  EXPECT_DOUBLE_EQ(evaluate("(x * y) > y"), 1.0);
  EXPECT_DOUBLE_EQ(evaluate("(x * y) > x"), 1.0);
}

TEST_F(VariableExpressionTest, LogicalWithComparison) {
  set_xy(2.2, 3.3);

  // x + y > x && x + y > y
  EXPECT_DOUBLE_EQ(evaluate("(x + y > x) && (x + y > y)"), 1.0);
}

// Power operations with variables
TEST_F(VariableExpressionTest, PowerOperations) {
  set_xy(12.34, 0.0);

  EXPECT_NEAR(evaluate("x**0"), 1.0, 1e-10);
  EXPECT_NEAR(evaluate("x**1"), 12.34, 1e-10);
  EXPECT_NEAR(evaluate("x**2"), 12.34 * 12.34, 1e-8);
  EXPECT_NEAR(evaluate("x**3"), std::pow(12.34, 3), 1e-6);
  EXPECT_NEAR(evaluate("x**4"), std::pow(12.34, 4), 1e-4);
}

TEST_F(VariableExpressionTest, NegativePowers) {
  set_xy(12.34, 0.0);

  EXPECT_NEAR(evaluate("x**(-1)"), 1.0 / 12.34, 1e-10);
  EXPECT_NEAR(evaluate("x**(-2)"), 1.0 / (12.34 * 12.34), 1e-10);
  EXPECT_NEAR(evaluate("x**(-3)"), 1.0 / std::pow(12.34, 3), 1e-10);
}

TEST_F(VariableExpressionTest, PowerAssociativity) {
  set_xy(3.3, 1.1);

  // Power is right-associative: x**2.2**y = x**(2.2**y)
  double expected = std::pow(3.3, std::pow(2.2, 1.1));
  EXPECT_NEAR(evaluate("x**2.2**y"), expected, 1e-8);
}

// =============================================================================
// User-defined function evaluation tests
// =============================================================================

class UserFunctionEvaluationTest : public ::testing::Test {
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

  void define_function(const std::string& name, const std::vector<std::string>& params,
                       const std::string& body_str) {
    ExprNode* body = parse_expression(body_str, arena);
    functions.register_user_function(std::make_unique<UserFunction>(name, params, body));
  }
};

TEST_F(UserFunctionEvaluationTest, SimpleSquare) {
  define_function("square", {"x"}, "x * x");

  EXPECT_DOUBLE_EQ(evaluate("square(2)"), 4.0);
  EXPECT_DOUBLE_EQ(evaluate("square(3)"), 9.0);
  EXPECT_DOUBLE_EQ(evaluate("square(4)"), 16.0);
  EXPECT_DOUBLE_EQ(evaluate("square(-5)"), 25.0);
}

TEST_F(UserFunctionEvaluationTest, SimpleCube) {
  define_function("cube", {"x"}, "x * x * x");

  EXPECT_DOUBLE_EQ(evaluate("cube(2)"), 8.0);
  EXPECT_DOUBLE_EQ(evaluate("cube(3)"), 27.0);
  EXPECT_DOUBLE_EQ(evaluate("cube(-2)"), -8.0);
}

TEST_F(UserFunctionEvaluationTest, TwoArguments) {
  define_function("add", {"a", "b"}, "a + b");
  define_function("mul", {"a", "b"}, "a * b");
  define_function("avg", {"a", "b"}, "(a + b) / 2");

  EXPECT_DOUBLE_EQ(evaluate("add(2, 3)"), 5.0);
  EXPECT_DOUBLE_EQ(evaluate("mul(2, 3)"), 6.0);
  EXPECT_DOUBLE_EQ(evaluate("avg(2, 4)"), 3.0);
}

TEST_F(UserFunctionEvaluationTest, ThreeArguments) {
  define_function("sum3", {"a", "b", "c"}, "a + b + c");
  define_function("avg3", {"a", "b", "c"}, "(a + b + c) / 3");

  EXPECT_DOUBLE_EQ(evaluate("sum3(1, 2, 3)"), 6.0);
  EXPECT_DOUBLE_EQ(evaluate("avg3(1, 2, 3)"), 2.0);
}

TEST_F(UserFunctionEvaluationTest, FunctionWithBuiltins) {
  define_function("hypotenuse", {"a", "b"}, "sqrt(a*a + b*b)");

  EXPECT_DOUBLE_EQ(evaluate("hypotenuse(3, 4)"), 5.0);
  EXPECT_NEAR(evaluate("hypotenuse(1, 1)"), std::sqrt(2.0), 1e-10);
}

TEST_F(UserFunctionEvaluationTest, NestedFunctionCalls) {
  define_function("double", {"x"}, "x * 2");
  define_function("triple", {"x"}, "x * 3");

  EXPECT_DOUBLE_EQ(evaluate("double(triple(2))"), 12.0);
  EXPECT_DOUBLE_EQ(evaluate("triple(double(2))"), 12.0);
  EXPECT_DOUBLE_EQ(evaluate("double(double(double(2)))"), 16.0);
}

TEST_F(UserFunctionEvaluationTest, FunctionInExpression) {
  define_function("square", {"x"}, "x * x");

  EXPECT_DOUBLE_EQ(evaluate("square(2) + square(3)"), 13.0);
  EXPECT_DOUBLE_EQ(evaluate("square(2) * square(3)"), 36.0);
  EXPECT_DOUBLE_EQ(evaluate("square(2 + 3)"), 25.0);
}

TEST_F(UserFunctionEvaluationTest, ConditionalInFunction) {
  define_function("abs_custom", {"x"}, "x >= 0 ? x : -x");

  EXPECT_DOUBLE_EQ(evaluate("abs_custom(5)"), 5.0);
  EXPECT_DOUBLE_EQ(evaluate("abs_custom(-5)"), 5.0);
  EXPECT_DOUBLE_EQ(evaluate("abs_custom(0)"), 0.0);
}

TEST_F(UserFunctionEvaluationTest, Polynomial) {
  // f(x) = 2*x^2 + 3*x + 1
  define_function("poly", {"x"}, "2*x**2 + 3*x + 1");

  EXPECT_DOUBLE_EQ(evaluate("poly(0)"), 1.0);
  EXPECT_DOUBLE_EQ(evaluate("poly(1)"), 6.0);
  EXPECT_DOUBLE_EQ(evaluate("poly(2)"), 15.0);
  EXPECT_DOUBLE_EQ(evaluate("poly(-1)"), 0.0);
}

TEST_F(UserFunctionEvaluationTest, MultiVariatePolynomial) {
  // f(x,y) = x^2 + 2*x*y + y^2 = (x+y)^2
  define_function("poly2", {"x", "y"}, "x**2 + 2*x*y + y**2");

  EXPECT_DOUBLE_EQ(evaluate("poly2(1, 1)"), 4.0);
  EXPECT_DOUBLE_EQ(evaluate("poly2(2, 3)"), 25.0);
  EXPECT_DOUBLE_EQ(evaluate("poly2(1, -1)"), 0.0);
}

// =============================================================================
// Complex arithmetic tests (adapted from exprtk_functional_test.txt)
// =============================================================================

class ComplexArithmeticTest : public ::testing::Test {
 protected:
  ExprArena arena;
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;

  void SetUp() override {
    // Set up x, y, z, w with specific values from ExprTK functional tests
    symbols.define("x", arena.make<NumberLiteral>(11.12345678910737373));
    symbols.define("y", arena.make<NumberLiteral>(22.12345678910737373));
    symbols.define("z", arena.make<NumberLiteral>(33.12345678910737373));
    symbols.define("w", arena.make<NumberLiteral>(44.12345678910737373));
  }

  double evaluate(const std::string& expr_str) {
    ExprNode* expr = parse_expression(expr_str, arena);
    RealEvaluator eval(symbols, functions, circuit);
    return eval.evaluate(*expr);
  }

  bool near_equal(double a, double b, double epsilon = 1e-10) {
    if (std::isnan(a) || std::isnan(b)) return false;
    double diff = std::abs(a - b);
    double norm = std::max(1.0, std::max(std::abs(a), std::abs(b)));
    return diff <= epsilon * norm;
  }
};

TEST_F(ComplexArithmeticTest, BasicFourVarArithmetic) {
  double x = 11.12345678910737373;
  double y = 22.12345678910737373;
  double z = 33.12345678910737373;

  EXPECT_TRUE(near_equal(evaluate("(((x+y)/z))"), (((x + y) / z))));
  EXPECT_TRUE(near_equal(evaluate("(((x+y)*z))"), (((x + y) * z))));
  EXPECT_TRUE(near_equal(evaluate("(((x+y)-z))"), (((x + y) - z))));
  EXPECT_TRUE(near_equal(evaluate("(((x+y)+z))"), (((x + y) + z))));
  EXPECT_TRUE(near_equal(evaluate("(((x-y)/z))"), (((x - y) / z))));
  EXPECT_TRUE(near_equal(evaluate("(((x-y)*z))"), (((x - y) * z))));
  EXPECT_TRUE(near_equal(evaluate("(((x*y)+z))"), (((x * y) + z))));
  EXPECT_TRUE(near_equal(evaluate("(((x*y)-z))"), (((x * y) - z))));
  EXPECT_TRUE(near_equal(evaluate("(((x*y)/z))"), (((x * y) / z))));
  EXPECT_TRUE(near_equal(evaluate("(((x*y)*z))"), (((x * y) * z))));
  EXPECT_TRUE(near_equal(evaluate("(((x/y)+z))"), (((x / y) + z))));
  EXPECT_TRUE(near_equal(evaluate("(((x/y)-z))"), (((x / y) - z))));
  EXPECT_TRUE(near_equal(evaluate("(((x/y)/z))"), (((x / y) / z))));
  EXPECT_TRUE(near_equal(evaluate("(((x/y)*z))"), (((x / y) * z))));
}

TEST_F(ComplexArithmeticTest, NestedDivision) {
  double x = 11.12345678910737373;
  double y = 22.12345678910737373;
  double z = 33.12345678910737373;

  EXPECT_TRUE(near_equal(evaluate("((x/(y+z)))"), ((x / (y + z)))));
  EXPECT_TRUE(near_equal(evaluate("((x/(y-z)))"), ((x / (y - z)))));
  EXPECT_TRUE(near_equal(evaluate("((x/(y*z)))"), ((x / (y * z)))));
  EXPECT_TRUE(near_equal(evaluate("((x/(y/z)))"), ((x / (y / z)))));
}

TEST_F(ComplexArithmeticTest, NestedMultiplication) {
  double x = 11.12345678910737373;
  double y = 22.12345678910737373;
  double z = 33.12345678910737373;

  EXPECT_TRUE(near_equal(evaluate("((x*(y+z)))"), ((x * (y + z)))));
  EXPECT_TRUE(near_equal(evaluate("((x*(y-z)))"), ((x * (y - z)))));
  EXPECT_TRUE(near_equal(evaluate("((x*(y*z)))"), ((x * (y * z)))));
  EXPECT_TRUE(near_equal(evaluate("((x*(y/z)))"), ((x * (y / z)))));
}

TEST_F(ComplexArithmeticTest, SubtractionWithNested) {
  double x = 11.12345678910737373;
  double y = 22.12345678910737373;
  double z = 33.12345678910737373;

  EXPECT_TRUE(near_equal(evaluate("((x-(y+z)))"), ((x - (y + z)))));
  EXPECT_TRUE(near_equal(evaluate("((x-(y-z)))"), ((x - (y - z)))));
  EXPECT_TRUE(near_equal(evaluate("((x-(y/z)))"), ((x - (y / z)))));
  EXPECT_TRUE(near_equal(evaluate("((x-(y*z)))"), ((x - (y * z)))));
}

TEST_F(ComplexArithmeticTest, AdditionWithNested) {
  double x = 11.12345678910737373;
  double y = 22.12345678910737373;
  double z = 33.12345678910737373;

  EXPECT_TRUE(near_equal(evaluate("((x+(y*z)))"), ((x + (y * z)))));
  EXPECT_TRUE(near_equal(evaluate("((x+(y/z)))"), ((x + (y / z)))));
  EXPECT_TRUE(near_equal(evaluate("((x+(y+z)))"), ((x + (y + z)))));
  EXPECT_TRUE(near_equal(evaluate("((x+(y-z)))"), ((x + (y - z)))));
}

TEST_F(ComplexArithmeticTest, FourVariableComplex) {
  double x = 11.12345678910737373;
  double y = 22.12345678910737373;
  double z = 33.12345678910737373;
  double w = 44.12345678910737373;

  EXPECT_TRUE(near_equal(evaluate("((x+((y+z)/w)))"), ((x + ((y + z) / w)))));
  EXPECT_TRUE(near_equal(evaluate("((x+((y+z)*w)))"), ((x + ((y + z) * w)))));
  EXPECT_TRUE(near_equal(evaluate("((x+((y-z)/w)))"), ((x + ((y - z) / w)))));
  EXPECT_TRUE(near_equal(evaluate("((x+((y-z)*w)))"), ((x + ((y - z) * w)))));
  EXPECT_TRUE(near_equal(evaluate("((x+((y*z)/w)))"), ((x + ((y * z) / w)))));
  EXPECT_TRUE(near_equal(evaluate("((x+((y*z)*w)))"), ((x + ((y * z) * w)))));
  EXPECT_TRUE(near_equal(evaluate("((x+((y/z)+w)))"), ((x + ((y / z) + w)))));
  EXPECT_TRUE(near_equal(evaluate("((x+((y/z)/w)))"), ((x + ((y / z) / w)))));
  EXPECT_TRUE(near_equal(evaluate("((x+((y/z)*w)))"), ((x + ((y / z) * w)))));
}

TEST_F(ComplexArithmeticTest, FourVariableSubtract) {
  double x = 11.12345678910737373;
  double y = 22.12345678910737373;
  double z = 33.12345678910737373;
  double w = 44.12345678910737373;

  EXPECT_TRUE(near_equal(evaluate("((x-((y+z)/w)))"), ((x - ((y + z) / w)))));
  EXPECT_TRUE(near_equal(evaluate("((x-((y+z)*w)))"), ((x - ((y + z) * w)))));
  EXPECT_TRUE(near_equal(evaluate("((x-((y-z)/w)))"), ((x - ((y - z) / w)))));
  EXPECT_TRUE(near_equal(evaluate("((x-((y-z)*w)))"), ((x - ((y - z) * w)))));
  EXPECT_TRUE(near_equal(evaluate("((x-((y*z)/w)))"), ((x - ((y * z) / w)))));
  EXPECT_TRUE(near_equal(evaluate("((x-((y*z)*w)))"), ((x - ((y * z) * w)))));
  EXPECT_TRUE(near_equal(evaluate("((x-((y/z)/w)))"), ((x - ((y / z) / w)))));
  EXPECT_TRUE(near_equal(evaluate("((x-((y/z)*w)))"), ((x - ((y / z) * w)))));
}

TEST_F(ComplexArithmeticTest, ChainedOperations) {
  double x = 11.12345678910737373;
  double y = 22.12345678910737373;
  double z = 33.12345678910737373;
  double w = 44.12345678910737373;

  EXPECT_TRUE(near_equal(evaluate("((((x+y)*z)-w))"), ((((x + y) * z) - w))));
  EXPECT_TRUE(near_equal(evaluate("((((x-y)*z)-w))"), ((((x - y) * z) - w))));
  EXPECT_TRUE(near_equal(evaluate("((((x*y)*z)-w))"), ((((x * y) * z) - w))));
  EXPECT_TRUE(near_equal(evaluate("((((x/y)*z)-w))"), ((((x / y) * z) - w))));
  EXPECT_TRUE(near_equal(evaluate("((((x+y)/z)-w))"), ((((x + y) / z) - w))));
  EXPECT_TRUE(near_equal(evaluate("((((x-y)/z)-w))"), ((((x - y) / z) - w))));
  EXPECT_TRUE(near_equal(evaluate("((((x*y)/z)-w))"), ((((x * y) / z) - w))));
  EXPECT_TRUE(near_equal(evaluate("((((x/y)/z)-w))"), ((((x / y) / z) - w))));
}

TEST_F(ComplexArithmeticTest, CrossProducts) {
  double x = 11.12345678910737373;
  double y = 22.12345678910737373;
  double z = 33.12345678910737373;
  double w = 44.12345678910737373;

  EXPECT_TRUE(near_equal(evaluate("(((x*y)+(z*w)))"), (((x * y) + (z * w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x*y)-(z*w)))"), (((x * y) - (z * w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x*y)+(z/w)))"), (((x * y) + (z / w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x*y)-(z/w)))"), (((x * y) - (z / w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x/y)+(z/w)))"), (((x / y) + (z / w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x/y)-(z/w)))"), (((x / y) - (z / w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x/y)-(z*w)))"), (((x / y) - (z * w)))));
}

TEST_F(ComplexArithmeticTest, DeeplyNestedExpressions) {
  double x = 11.12345678910737373;
  double y = 22.12345678910737373;
  double z = 33.12345678910737373;
  double w = 44.12345678910737373;

  EXPECT_TRUE(near_equal(evaluate("((x/(y+(z*w))))"), ((x / (y + (z * w))))));
  EXPECT_TRUE(near_equal(evaluate("((x/(y-(z*w))))"), ((x / (y - (z * w))))));
  EXPECT_TRUE(near_equal(evaluate("((x*(y+(z*w))))"), ((x * (y + (z * w))))));
  EXPECT_TRUE(near_equal(evaluate("((x*(y-(z*w))))"), ((x * (y - (z * w))))));
}

TEST_F(ComplexArithmeticTest, SumDifferenceProducts) {
  double x = 11.12345678910737373;
  double y = 22.12345678910737373;
  double z = 33.12345678910737373;
  double w = 44.12345678910737373;

  EXPECT_TRUE(near_equal(evaluate("(((x+y)-(z*w)))"), (((x + y) - (z * w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x+y)-(z/w)))"), (((x + y) - (z / w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x+y)+(z*w)))"), (((x + y) + (z * w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x+y)+(z/w)))"), (((x + y) + (z / w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x-y)+(z*w)))"), (((x - y) + (z * w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x-y)+(z/w)))"), (((x - y) + (z / w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x-y)-(z*w)))"), (((x - y) - (z * w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x-y)-(z/w)))"), (((x - y) - (z / w)))));
}

TEST_F(ComplexArithmeticTest, WithAddSubGroups) {
  double x = 11.12345678910737373;
  double y = 22.12345678910737373;
  double z = 33.12345678910737373;
  double w = 44.12345678910737373;

  EXPECT_TRUE(near_equal(evaluate("(((x+y)-(z-w)))"), (((x + y) - (z - w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x+y)+(z-w)))"), (((x + y) + (z - w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x+y)*(z-w)))"), (((x + y) * (z - w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x+y)/(z-w)))"), (((x + y) / (z - w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x-y)-(z+w)))"), (((x - y) - (z + w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x-y)+(z+w)))"), (((x - y) + (z + w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x-y)*(z+w)))"), (((x - y) * (z + w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x-y)/(z+w)))"), (((x - y) / (z + w)))));
}

TEST_F(ComplexArithmeticTest, ProductSumMixed) {
  double x = 11.12345678910737373;
  double y = 22.12345678910737373;
  double z = 33.12345678910737373;
  double w = 44.12345678910737373;

  EXPECT_TRUE(near_equal(evaluate("(((x*y)-(z+w)))"), (((x * y) - (z + w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x/y)-(z+w)))"), (((x / y) - (z + w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x*y)+(z+w)))"), (((x * y) + (z + w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x/y)+(z+w)))"), (((x / y) + (z + w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x*y)+(z-w)))"), (((x * y) + (z - w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x/y)+(z-w)))"), (((x / y) + (z - w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x*y)-(z-w)))"), (((x * y) - (z - w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x/y)-(z-w)))"), (((x / y) - (z - w)))));
}

TEST_F(ComplexArithmeticTest, AllOperationsChained) {
  double x = 11.12345678910737373;
  double y = 22.12345678910737373;
  double z = 33.12345678910737373;
  double w = 44.12345678910737373;

  EXPECT_TRUE(near_equal(evaluate("(((x+y)*(z*w)))"), (((x + y) * (z * w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x+y)*(z/w)))"), (((x + y) * (z / w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x+y)/(z*w)))"), (((x + y) / (z * w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x+y)/(z/w)))"), (((x + y) / (z / w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x-y)/(z*w)))"), (((x - y) / (z * w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x-y)/(z/w)))"), (((x - y) / (z / w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x-y)*(z*w)))"), (((x - y) * (z * w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x-y)*(z/w)))"), (((x - y) * (z / w)))));
}

TEST_F(ComplexArithmeticTest, ProductDivisionMixed) {
  double x = 11.12345678910737373;
  double y = 22.12345678910737373;
  double z = 33.12345678910737373;
  double w = 44.12345678910737373;

  EXPECT_TRUE(near_equal(evaluate("(((x*y)*(z+w)))"), (((x * y) * (z + w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x/y)*(z+w)))"), (((x / y) * (z + w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x*y)/(z+w)))"), (((x * y) / (z + w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x/y)/(z+w)))"), (((x / y) / (z + w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x*y)/(z-w)))"), (((x * y) / (z - w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x/y)/(z-w)))"), (((x / y) / (z - w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x*y)*(z-w)))"), (((x * y) * (z - w)))));
  EXPECT_TRUE(near_equal(evaluate("(((x/y)*(z-w)))"), (((x / y) * (z - w)))));
}

// =============================================================================
// Trigonometric function tests with variables
// =============================================================================

class TrigonometricVariableTest : public ::testing::Test {
 protected:
  ExprArena arena;
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;

  void SetUp() override {
    symbols.define("x", arena.make<NumberLiteral>(0.0));
    symbols.define("y", arena.make<NumberLiteral>(0.0));
  }

  void set_xy(double x_val, double y_val) {
    symbols.set_override("x", arena.make<NumberLiteral>(x_val));
    symbols.set_override("y", arena.make<NumberLiteral>(y_val));
  }

  double evaluate(const std::string& expr_str) {
    ExprNode* expr = parse_expression(expr_str, arena);
    RealEvaluator eval(symbols, functions, circuit);
    return eval.evaluate(*expr);
  }
};

TEST_F(TrigonometricVariableTest, SinCosIdentity) {
  // sin^2(x) + cos^2(x) = 1
  for (double x = -3.14; x <= 3.14; x += 0.5) {
    set_xy(x, 0.0);
    EXPECT_NEAR(evaluate("sin(x)**2 + cos(x)**2"), 1.0, 1e-10) << "x=" << x;
  }
}

TEST_F(TrigonometricVariableTest, SinAdditionFormula) {
  // sin(x+y) = sin(x)*cos(y) + cos(x)*sin(y)
  for (double x = -1.5; x <= 1.5; x += 0.5) {
    for (double y = -1.5; y <= 1.5; y += 0.5) {
      set_xy(x, y);
      double lhs = evaluate("sin(x + y)");
      double rhs = evaluate("sin(x)*cos(y) + cos(x)*sin(y)");
      EXPECT_NEAR(lhs, rhs, 1e-10) << "x=" << x << " y=" << y;
    }
  }
}

TEST_F(TrigonometricVariableTest, CosAdditionFormula) {
  // cos(x+y) = cos(x)*cos(y) - sin(x)*sin(y)
  for (double x = -1.5; x <= 1.5; x += 0.5) {
    for (double y = -1.5; y <= 1.5; y += 0.5) {
      set_xy(x, y);
      double lhs = evaluate("cos(x + y)");
      double rhs = evaluate("cos(x)*cos(y) - sin(x)*sin(y)");
      EXPECT_NEAR(lhs, rhs, 1e-10) << "x=" << x << " y=" << y;
    }
  }
}

TEST_F(TrigonometricVariableTest, DoubleAngle) {
  // sin(2x) = 2*sin(x)*cos(x)
  for (double x = -1.5; x <= 1.5; x += 0.25) {
    set_xy(x, 0.0);
    EXPECT_NEAR(evaluate("sin(2*x)"), evaluate("2*sin(x)*cos(x)"), 1e-10) << "x=" << x;
  }
}

TEST_F(TrigonometricVariableTest, TanIdentity) {
  // tan(x) = sin(x)/cos(x)
  for (double x = -1.4; x <= 1.4; x += 0.2) {
    set_xy(x, 0.0);
    EXPECT_NEAR(evaluate("tan(x)"), evaluate("sin(x)/cos(x)"), 1e-10) << "x=" << x;
  }
}

// =============================================================================
// Exponential and logarithmic tests with variables
// =============================================================================

class ExpLogVariableTest : public ::testing::Test {
 protected:
  ExprArena arena;
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;

  void SetUp() override {
    symbols.define("x", arena.make<NumberLiteral>(1.0));
    symbols.define("y", arena.make<NumberLiteral>(1.0));
  }

  void set_xy(double x_val, double y_val) {
    symbols.set_override("x", arena.make<NumberLiteral>(x_val));
    symbols.set_override("y", arena.make<NumberLiteral>(y_val));
  }

  double evaluate(const std::string& expr_str) {
    ExprNode* expr = parse_expression(expr_str, arena);
    RealEvaluator eval(symbols, functions, circuit);
    return eval.evaluate(*expr);
  }
};

TEST_F(ExpLogVariableTest, ExpLogInverse) {
  // exp(log(x)) = x for x > 0
  for (double x = 0.1; x <= 5.0; x += 0.5) {
    set_xy(x, 0.0);
    EXPECT_NEAR(evaluate("exp(log(x))"), x, 1e-10) << "x=" << x;
  }
}

TEST_F(ExpLogVariableTest, LogExpInverse) {
  // log(exp(x)) = x
  for (double x = -2.0; x <= 2.0; x += 0.5) {
    set_xy(x, 0.0);
    EXPECT_NEAR(evaluate("log(exp(x))"), x, 1e-10) << "x=" << x;
  }
}

TEST_F(ExpLogVariableTest, LogProductRule) {
  // log(x*y) = log(x) + log(y) for x,y > 0
  for (double x = 0.5; x <= 3.0; x += 0.5) {
    for (double y = 0.5; y <= 3.0; y += 0.5) {
      set_xy(x, y);
      EXPECT_NEAR(evaluate("log(x*y)"), evaluate("log(x) + log(y)"), 1e-10)
          << "x=" << x << " y=" << y;
    }
  }
}

TEST_F(ExpLogVariableTest, LogQuotientRule) {
  // log(x/y) = log(x) - log(y) for x,y > 0
  for (double x = 0.5; x <= 3.0; x += 0.5) {
    for (double y = 0.5; y <= 3.0; y += 0.5) {
      set_xy(x, y);
      EXPECT_NEAR(evaluate("log(x/y)"), evaluate("log(x) - log(y)"), 1e-10)
          << "x=" << x << " y=" << y;
    }
  }
}

TEST_F(ExpLogVariableTest, ExpSumRule) {
  // exp(x+y) = exp(x)*exp(y)
  for (double x = -1.0; x <= 1.0; x += 0.5) {
    for (double y = -1.0; y <= 1.0; y += 0.5) {
      set_xy(x, y);
      EXPECT_NEAR(evaluate("exp(x+y)"), evaluate("exp(x)*exp(y)"), 1e-10)
          << "x=" << x << " y=" << y;
    }
  }
}

TEST_F(ExpLogVariableTest, Log10Identity) {
  // log10(10^x) = x
  for (double x = -2.0; x <= 2.0; x += 0.5) {
    set_xy(x, 0.0);
    EXPECT_NEAR(evaluate("log10(10**x)"), x, 1e-10) << "x=" << x;
  }
}

// =============================================================================
// Function composition tests (adapted from ExprTK run_test19)
// =============================================================================

class FunctionCompositionTest : public ::testing::Test {
 protected:
  ExprArena arena;
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;

  void define_function(const std::string& name, const std::vector<std::string>& params,
                       const std::string& body_str) {
    ExprNode* body = parse_expression(body_str, arena);
    functions.register_user_function(std::make_unique<UserFunction>(name, params, body));
  }

  double evaluate(const std::string& expr_str) {
    ExprNode* expr = parse_expression(expr_str, arena);
    RealEvaluator eval(symbols, functions, circuit);
    return eval.evaluate(*expr);
  }
};

// Basic function composition: f(x) = x + 2, g(x) = x^2 - 3
TEST_F(FunctionCompositionTest, BasicComposition) {
  // f(x) = x + 2
  define_function("f", {"x"}, "x + 2");

  // g(x) = x^2 - 3
  define_function("g", {"x"}, "x**2 - 3");

  // Test basic function calls
  EXPECT_DOUBLE_EQ(evaluate("f(1)"), 3.0);   // 1 + 2 = 3
  EXPECT_DOUBLE_EQ(evaluate("f(5)"), 7.0);   // 5 + 2 = 7
  EXPECT_DOUBLE_EQ(evaluate("g(2)"), 1.0);   // 2^2 - 3 = 1
  EXPECT_DOUBLE_EQ(evaluate("g(3)"), 6.0);   // 3^2 - 3 = 6
}

// f(f(x)) composition
TEST_F(FunctionCompositionTest, FofComposition) {
  define_function("f", {"x"}, "x + 2");

  // f(f(x)) = f(x + 2) = (x + 2) + 2 = x + 4
  // Define fof as a composite function
  define_function("fof", {"x"}, "f(f(x))");

  EXPECT_DOUBLE_EQ(evaluate("fof(1)"), 5.0);   // 1 + 4 = 5
  EXPECT_DOUBLE_EQ(evaluate("fof(10)"), 14.0); // 10 + 4 = 14

  // Also test direct nested call
  EXPECT_DOUBLE_EQ(evaluate("f(f(1))"), 5.0);
  EXPECT_DOUBLE_EQ(evaluate("f(f(10))"), 14.0);
}

// g(g(x)) composition
TEST_F(FunctionCompositionTest, GogComposition) {
  define_function("g", {"x"}, "x**2 - 3");

  // g(g(x)) = g(x^2 - 3) = (x^2 - 3)^2 - 3 = x^4 - 6x^2 + 9 - 3 = x^4 - 6x^2 + 6
  define_function("gog", {"x"}, "g(g(x))");

  // Test with x = 2: gog(2) = 2^4 - 6*2^2 + 6 = 16 - 24 + 6 = -2
  EXPECT_DOUBLE_EQ(evaluate("gog(2)"), -2.0);

  // Test with x = 3: gog(3) = 3^4 - 6*3^2 + 6 = 81 - 54 + 6 = 33
  EXPECT_DOUBLE_EQ(evaluate("gog(3)"), 33.0);

  // Direct nested call
  EXPECT_DOUBLE_EQ(evaluate("g(g(2))"), -2.0);
}

// f(g(x)) composition
TEST_F(FunctionCompositionTest, FogComposition) {
  define_function("f", {"x"}, "x + 2");
  define_function("g", {"x"}, "x**2 - 3");

  // fog(x) = f(g(x)) = f(x^2 - 3) = (x^2 - 3) + 2 = x^2 - 1
  define_function("fog", {"x"}, "f(g(x))");

  EXPECT_DOUBLE_EQ(evaluate("fog(2)"), 3.0);  // 2^2 - 1 = 3
  EXPECT_DOUBLE_EQ(evaluate("fog(3)"), 8.0);  // 3^2 - 1 = 8
  EXPECT_DOUBLE_EQ(evaluate("fog(5)"), 24.0); // 5^2 - 1 = 24

  // Direct nested call
  EXPECT_DOUBLE_EQ(evaluate("f(g(2))"), 3.0);
}

// g(f(x)) composition
TEST_F(FunctionCompositionTest, GofComposition) {
  define_function("f", {"x"}, "x + 2");
  define_function("g", {"x"}, "x**2 - 3");

  // gof(x) = g(f(x)) = g(x + 2) = (x + 2)^2 - 3 = x^2 + 4x + 4 - 3 = x^2 + 4x + 1
  define_function("gof", {"x"}, "g(f(x))");

  // x=1: 1 + 4 + 1 = 6
  EXPECT_DOUBLE_EQ(evaluate("gof(1)"), 6.0);

  // x=2: 4 + 8 + 1 = 13
  EXPECT_DOUBLE_EQ(evaluate("gof(2)"), 13.0);

  // x=3: 9 + 12 + 1 = 22
  EXPECT_DOUBLE_EQ(evaluate("gof(3)"), 22.0);

  // Direct nested call
  EXPECT_DOUBLE_EQ(evaluate("g(f(1))"), 6.0);
}

// Triple composition: f(g(f(x)))
TEST_F(FunctionCompositionTest, FogofComposition) {
  define_function("f", {"x"}, "x + 2");
  define_function("g", {"x"}, "x**2 - 3");

  // fogof(x) = f(g(f(x))) = f(g(x+2)) = f((x+2)^2 - 3) = (x+2)^2 - 3 + 2 = x^2 + 4x + 4 - 1 = x^2 + 4x + 3
  define_function("fogof", {"x"}, "f(g(f(x)))");

  // x=1: 1 + 4 + 3 = 8
  EXPECT_DOUBLE_EQ(evaluate("fogof(1)"), 8.0);

  // x=2: 4 + 8 + 3 = 15
  EXPECT_DOUBLE_EQ(evaluate("fogof(2)"), 15.0);

  // Direct nested call
  EXPECT_DOUBLE_EQ(evaluate("f(g(f(1)))"), 8.0);
}

// Triple composition: g(f(g(x)))
TEST_F(FunctionCompositionTest, GofogComposition) {
  define_function("f", {"x"}, "x + 2");
  define_function("g", {"x"}, "x**2 - 3");

  // gofog(x) = g(f(g(x))) = g(f(x^2-3)) = g(x^2-3+2) = g(x^2-1) = (x^2-1)^2 - 3
  // = x^4 - 2x^2 + 1 - 3 = x^4 - 2x^2 - 2
  define_function("gofog", {"x"}, "g(f(g(x)))");

  // x=2: 16 - 8 - 2 = 6
  EXPECT_DOUBLE_EQ(evaluate("gofog(2)"), 6.0);

  // x=3: 81 - 18 - 2 = 61
  EXPECT_DOUBLE_EQ(evaluate("gofog(3)"), 61.0);

  // Direct nested call
  EXPECT_DOUBLE_EQ(evaluate("g(f(g(2)))"), 6.0);
}

// Multi-parameter function composition
TEST_F(FunctionCompositionTest, MultiParamComposition) {
  // Define functions with multiple parameters
  define_function("add", {"a", "b"}, "a + b");
  define_function("mul", {"a", "b"}, "a * b");

  // Compose them
  define_function("add_then_mul", {"a", "b", "c"}, "mul(add(a, b), c)");
  define_function("mul_then_add", {"a", "b", "c"}, "add(mul(a, b), c)");

  // add_then_mul(1, 2, 3) = mul(add(1, 2), 3) = mul(3, 3) = 9
  EXPECT_DOUBLE_EQ(evaluate("add_then_mul(1, 2, 3)"), 9.0);

  // mul_then_add(2, 3, 4) = add(mul(2, 3), 4) = add(6, 4) = 10
  EXPECT_DOUBLE_EQ(evaluate("mul_then_add(2, 3, 4)"), 10.0);
}

// Chain of function compositions (similar to ExprTK f0..f6)
TEST_F(FunctionCompositionTest, FunctionChain) {
  // f0() = 6 (constant function, we simulate with a parameter)
  // Since spice_expr doesn't support zero-arg user functions directly,
  // we define it as a single-arg function that ignores the argument
  define_function("f0", {"_"}, "3 * 2");

  // f1(x) = 5 * (f0(0) + x)
  define_function("f1", {"x"}, "5 * (f0(0) + x)");

  // f1(1) = 5 * (6 + 1) = 35
  EXPECT_DOUBLE_EQ(evaluate("f1(1)"), 35.0);

  // f2(x, y) = 7 * (f1(x) + f1(y))
  define_function("f2", {"x", "y"}, "7 * (f1(x) + f1(y))");

  // f2(1, 1) = 7 * (f1(1) + f1(1)) = 7 * (35 + 35) = 490
  EXPECT_DOUBLE_EQ(evaluate("f2(1, 1)"), 490.0);

  // f2(1, 2) = 7 * (f1(1) + f1(2)) = 7 * (35 + 40) = 7 * 75 = 525
  EXPECT_DOUBLE_EQ(evaluate("f2(1, 2)"), 525.0);
}

// Function with builtin function calls inside composition
TEST_F(FunctionCompositionTest, BuiltinInComposition) {
  define_function("safe_sqrt", {"x"}, "x >= 0 ? sqrt(x) : 0");
  define_function("compose_sqrt", {"x"}, "safe_sqrt(safe_sqrt(x))");

  // compose_sqrt(16) = safe_sqrt(safe_sqrt(16)) = safe_sqrt(4) = 2
  EXPECT_DOUBLE_EQ(evaluate("compose_sqrt(16)"), 2.0);

  // compose_sqrt(81) = safe_sqrt(safe_sqrt(81)) = safe_sqrt(9) = 3
  EXPECT_DOUBLE_EQ(evaluate("compose_sqrt(81)"), 3.0);
}

// Function using trig functions in composition
TEST_F(FunctionCompositionTest, TrigComposition) {
  define_function("sincos", {"x"}, "sin(x) * cos(x)");
  define_function("double_sincos", {"x"}, "sincos(x) + sincos(x)");

  // double_sincos(x) = 2 * sin(x) * cos(x) = sin(2x)
  double x = 0.5;
  double expected = std::sin(2 * x);
  EXPECT_NEAR(evaluate("double_sincos(0.5)"), expected, 1e-10);
}

// Function composition with parameter variables
TEST_F(FunctionCompositionTest, CompositionWithSymbols) {
  symbols.define("a", arena.make<NumberLiteral>(10.0));
  symbols.define("b", arena.make<NumberLiteral>(20.0));

  define_function("scale", {"x", "factor"}, "x * factor");
  define_function("offset", {"x", "delta"}, "x + delta");

  // Test using defined symbols in function calls
  EXPECT_DOUBLE_EQ(evaluate("scale(5, 2)"), 10.0);
  EXPECT_DOUBLE_EQ(evaluate("offset(5, 3)"), 8.0);

  // Compose: offset(scale(x, 2), 5) = 2x + 5
  define_function("scale_then_offset", {"x"}, "offset(scale(x, 2), 5)");
  EXPECT_DOUBLE_EQ(evaluate("scale_then_offset(10)"), 25.0);  // 2*10 + 5 = 25
}

// Deep composition chain
TEST_F(FunctionCompositionTest, DeepCompositionChain) {
  define_function("inc", {"x"}, "x + 1");

  // Chain: inc(inc(inc(inc(inc(x)))))
  define_function("inc5", {"x"}, "inc(inc(inc(inc(inc(x)))))");

  EXPECT_DOUBLE_EQ(evaluate("inc5(0)"), 5.0);
  EXPECT_DOUBLE_EQ(evaluate("inc5(10)"), 15.0);

  // Even deeper: inc5(inc5(x)) = x + 10
  define_function("inc10", {"x"}, "inc5(inc5(x))");

  EXPECT_DOUBLE_EQ(evaluate("inc10(0)"), 10.0);
  EXPECT_DOUBLE_EQ(evaluate("inc10(5)"), 15.0);
}

// Composition with conditionals
TEST_F(FunctionCompositionTest, ConditionalComposition) {
  define_function("abs_val", {"x"}, "x >= 0 ? x : -x");
  define_function("sign", {"x"}, "x > 0 ? 1 : (x < 0 ? -1 : 0)");

  // Compose: sign(abs_val(x)) should always be 0 or 1
  define_function("sign_abs", {"x"}, "sign(abs_val(x))");

  EXPECT_DOUBLE_EQ(evaluate("sign_abs(5)"), 1.0);
  EXPECT_DOUBLE_EQ(evaluate("sign_abs(-5)"), 1.0);
  EXPECT_DOUBLE_EQ(evaluate("sign_abs(0)"), 0.0);
}

// Mutual recursion-like pattern (non-recursive, just mutual calls)
TEST_F(FunctionCompositionTest, MutualCalls) {
  // Note: This is not true mutual recursion, just functions calling each other
  define_function("double_it", {"x"}, "x * 2");
  define_function("halve_it", {"x"}, "x / 2");

  // double then halve should return original
  define_function("double_halve", {"x"}, "halve_it(double_it(x))");
  define_function("halve_double", {"x"}, "double_it(halve_it(x))");

  EXPECT_DOUBLE_EQ(evaluate("double_halve(7)"), 7.0);
  EXPECT_DOUBLE_EQ(evaluate("halve_double(7)"), 7.0);
  EXPECT_DOUBLE_EQ(evaluate("double_halve(123.456)"), 123.456);
}
