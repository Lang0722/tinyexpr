#include <gtest/gtest.h>

#include <cmath>
#include <string>
#include <vector>

#include "spice_expr/spice_expr.h"

using namespace spice_expr;

class StressTest : public ::testing::Test {
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

// Variable iteration test - iterate x,y over a grid and verify against native implementation
// Adapted from ExprTK test04
TEST_F(StressTest, TrigonometricIteration) {
  const double delta = 0.5;

  symbols.define("x", arena.make<NumberLiteral>(0.0));
  symbols.define("y", arena.make<NumberLiteral>(0.0));

  ExprNode* expr = parse_expression("sin(2 * x) + cos(y / 2)", arena);
  RealEvaluator eval(symbols, functions, circuit);

  for (double x = -5.0; x <= 5.0; x += delta) {
    for (double y = -5.0; y <= 5.0; y += delta) {
      symbols.set_override("x", arena.make<NumberLiteral>(x));
      symbols.set_override("y", arena.make<NumberLiteral>(y));

      double result = eval.evaluate(*expr);
      double expected = std::sin(2.0 * x) + std::cos(y / 2.0);

      EXPECT_NEAR(result, expected, 1e-10) << "x=" << x << " y=" << y;
    }
  }

  symbols.clear_override("x");
  symbols.clear_override("y");
}

// Test with multiple trigonometric functions
TEST_F(StressTest, ComplexTrigonometricIteration) {
  const double delta = 0.5;

  symbols.define("x", arena.make<NumberLiteral>(0.0));
  symbols.define("y", arena.make<NumberLiteral>(0.0));

  ExprNode* expr =
      parse_expression("sin(x) * cos(y) + cos(x) * sin(y) - sin(x + y)", arena);
  RealEvaluator eval(symbols, functions, circuit);

  for (double x = -3.0; x <= 3.0; x += delta) {
    for (double y = -3.0; y <= 3.0; y += delta) {
      symbols.set_override("x", arena.make<NumberLiteral>(x));
      symbols.set_override("y", arena.make<NumberLiteral>(y));

      double result = eval.evaluate(*expr);
      // sin(x)*cos(y) + cos(x)*sin(y) = sin(x+y), so result should be ~0
      EXPECT_NEAR(result, 0.0, 1e-10) << "x=" << x << " y=" << y;
    }
  }

  symbols.clear_override("x");
  symbols.clear_override("y");
}

// Large parameter set test
TEST_F(StressTest, ManyParameters) {
  const int num_params = 100;

  // Define 100 parameters p0, p1, ..., p99 with values 1, 2, ..., 100
  for (int i = 0; i < num_params; ++i) {
    std::string name = "p" + std::to_string(i);
    symbols.define(name, arena.make<NumberLiteral>(static_cast<double>(i + 1)));
  }

  // Build expression: p0 + p1 + ... + p99
  std::string expr_str = "p0";
  for (int i = 1; i < num_params; ++i) {
    expr_str += " + p" + std::to_string(i);
  }

  double result = evaluate(expr_str);

  // Sum of 1 to 100 = 100 * 101 / 2 = 5050
  EXPECT_DOUBLE_EQ(result, 5050.0);
}

// Deep nesting test with sin function
TEST_F(StressTest, DeepNesting) {
  const int depth = 20;

  symbols.define("x", arena.make<NumberLiteral>(0.5));

  std::string expr = "x";
  for (int i = 0; i < depth; ++i) {
    expr = "sin(" + expr + ")";
  }

  // Should not stack overflow
  double result = evaluate(expr);

  // Verify by computing manually
  double expected = 0.5;
  for (int i = 0; i < depth; ++i) {
    expected = std::sin(expected);
  }

  EXPECT_NEAR(result, expected, 1e-10);
}

// Deep nesting with nested parentheses
TEST_F(StressTest, DeepParenthesesNesting) {
  const int depth = 50;

  std::string expr = "1";
  for (int i = 0; i < depth; ++i) {
    expr = "(" + expr + ")";
  }

  // Should parse and evaluate correctly
  double result = evaluate(expr);
  EXPECT_DOUBLE_EQ(result, 1.0);
}

// Long dependency chain test
TEST_F(StressTest, LongDependencyChain) {
  const int chain_length = 50;

  // Create chain: p0 = 1, p1 = p0 + 1, p2 = p1 + 1, ... p49 = p48 + 1
  symbols.define("p0", arena.make<NumberLiteral>(1.0));

  for (int i = 1; i < chain_length; ++i) {
    auto* prev = arena.make<Identifier>("p" + std::to_string(i - 1));
    auto* one = arena.make<NumberLiteral>(1.0);
    auto* sum = arena.make<BinaryOp>(BinaryOpType::Add, prev, one);
    symbols.define("p" + std::to_string(i), sum);
  }

  // Evaluate p49, should equal 50
  ExprNode* expr = arena.make<Identifier>("p" + std::to_string(chain_length - 1));
  RealEvaluator eval(symbols, functions, circuit);
  double result = eval.evaluate(*expr);

  EXPECT_DOUBLE_EQ(result, static_cast<double>(chain_length));
}

// Binary tree dependency structure
TEST_F(StressTest, BinaryTreeDependencies) {
  // Create a binary tree of expressions:
  // Level 0: a0=1, a1=2, a2=3, a3=4, a4=5, a5=6, a6=7, a7=8
  // Level 1: b0=a0+a1, b1=a2+a3, b2=a4+a5, b3=a6+a7
  // Level 2: c0=b0+b1, c1=b2+b3
  // Level 3: d0=c0+c1

  for (int i = 0; i < 8; ++i) {
    std::string name = "a" + std::to_string(i);
    symbols.define(name, arena.make<NumberLiteral>(static_cast<double>(i + 1)));
  }

  // Level 1
  for (int i = 0; i < 4; ++i) {
    std::string name = "b" + std::to_string(i);
    auto* left = arena.make<Identifier>("a" + std::to_string(i * 2));
    auto* right = arena.make<Identifier>("a" + std::to_string(i * 2 + 1));
    auto* sum = arena.make<BinaryOp>(BinaryOpType::Add, left, right);
    symbols.define(name, sum);
  }

  // Level 2
  for (int i = 0; i < 2; ++i) {
    std::string name = "c" + std::to_string(i);
    auto* left = arena.make<Identifier>("b" + std::to_string(i * 2));
    auto* right = arena.make<Identifier>("b" + std::to_string(i * 2 + 1));
    auto* sum = arena.make<BinaryOp>(BinaryOpType::Add, left, right);
    symbols.define(name, sum);
  }

  // Level 3
  {
    auto* left = arena.make<Identifier>("c0");
    auto* right = arena.make<Identifier>("c1");
    auto* sum = arena.make<BinaryOp>(BinaryOpType::Add, left, right);
    symbols.define("d0", sum);
  }

  // d0 should equal 1+2+3+4+5+6+7+8 = 36
  ExprNode* expr = arena.make<Identifier>("d0");
  RealEvaluator eval(symbols, functions, circuit);
  double result = eval.evaluate(*expr);

  EXPECT_DOUBLE_EQ(result, 36.0);
}

// Large expression with many operators
TEST_F(StressTest, LargeExpression) {
  const int terms = 100;

  // Build: 1*x + 2*x + 3*x + ... + 100*x
  symbols.define("x", arena.make<NumberLiteral>(2.0));

  std::string expr;
  for (int i = 1; i <= terms; ++i) {
    if (i > 1) expr += " + ";
    expr += std::to_string(i) + "*x";
  }

  double result = evaluate(expr);

  // Sum of 1 to 100 = 5050, times x=2 = 10100
  EXPECT_DOUBLE_EQ(result, 10100.0);
}

// Mixed operations stress test
TEST_F(StressTest, MixedOperations) {
  symbols.define("a", arena.make<NumberLiteral>(2.0));
  symbols.define("b", arena.make<NumberLiteral>(3.0));
  symbols.define("c", arena.make<NumberLiteral>(4.0));

  // Complex expression with multiple operation types
  std::string expr =
      "((a + b) * c - a ** 2 + sin(b) * cos(c)) / "
      "(1 + abs(a - b) + sqrt(c)) + "
      "(a > b ? a : b) + min(a, b, c) + max(a, b, c)";

  double result = evaluate(expr);

  // Verify manually
  double a = 2.0, b = 3.0, c = 4.0;
  double expected = ((a + b) * c - std::pow(a, 2) + std::sin(b) * std::cos(c)) /
                        (1 + std::abs(a - b) + std::sqrt(c)) +
                    (a > b ? a : b) + std::min({a, b, c}) + std::max({a, b, c});

  EXPECT_NEAR(result, expected, 1e-10);
}

// Parameter override stress test
TEST_F(StressTest, ParameterOverrideStress) {
  symbols.define("x", arena.make<NumberLiteral>(1.0));

  ExprNode* expr = parse_expression("x * x", arena);
  RealEvaluator eval(symbols, functions, circuit);

  // Override many times
  for (int i = 1; i <= 100; ++i) {
    symbols.set_override("x", arena.make<NumberLiteral>(static_cast<double>(i)));
    double result = eval.evaluate(*expr);
    EXPECT_DOUBLE_EQ(result, static_cast<double>(i * i));
  }

  // Clear and verify original
  symbols.clear_override("x");
  double result = eval.evaluate(*expr);
  EXPECT_DOUBLE_EQ(result, 1.0);
}

// Deeply nested ternary expressions
TEST_F(StressTest, DeepTernary) {
  symbols.define("x", arena.make<NumberLiteral>(5.0));

  // Build nested ternary: x > 9 ? 9 : (x > 8 ? 8 : (x > 7 ? 7 : ... (x > 0 ? 0 : -1)))
  std::string expr = "-1";
  for (int i = 0; i <= 9; ++i) {
    expr = "(x > " + std::to_string(i) + " ? " + std::to_string(i) + " : " + expr + ")";
  }

  double result = evaluate(expr);
  EXPECT_DOUBLE_EQ(result, 4.0);  // x=5, so x > 4 is true, returns 4
}

// Long expression parsing
TEST_F(StressTest, LongExpressionParsing) {
  // Create a very long expression
  std::string expr;
  for (int i = 0; i < 500; ++i) {
    if (i > 0) expr += " + ";
    expr += std::to_string(i + 1);
  }

  double result = evaluate(expr);

  // Sum of 1 to 500 = 500 * 501 / 2 = 125250
  EXPECT_DOUBLE_EQ(result, 125250.0);
}

// Multiple circuit references
TEST_F(StressTest, ManyCircuitReferences) {
  MockCircuitInterface mock_circuit;

  // Set up many node voltages
  for (int i = 0; i < 20; ++i) {
    mock_circuit.set_node_voltage("n" + std::to_string(i), static_cast<double>(i));
  }

  // Build expression with many V() references
  std::string expr = "V(n0)";
  for (int i = 1; i < 20; ++i) {
    expr += " + V(n" + std::to_string(i) + ")";
  }

  ExprNode* parsed = parse_expression(expr, arena);
  RealEvaluator eval(symbols, functions, mock_circuit);
  double result = eval.evaluate(*parsed);

  // Sum of 0 to 19 = 19 * 20 / 2 = 190
  EXPECT_DOUBLE_EQ(result, 190.0);
}

// Repeated evaluations with same expression
TEST_F(StressTest, RepeatedEvaluations) {
  symbols.define("x", arena.make<NumberLiteral>(2.0));

  ExprNode* expr = parse_expression("x**2 + 2*x + 1", arena);
  RealEvaluator eval(symbols, functions, circuit);

  // Evaluate 1000 times
  for (int i = 0; i < 1000; ++i) {
    double result = eval.evaluate(*expr);
    EXPECT_DOUBLE_EQ(result, 9.0);  // (2+1)^2 = 9
  }
}
