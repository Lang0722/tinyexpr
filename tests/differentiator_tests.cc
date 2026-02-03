#include <gtest/gtest.h>

#include <cmath>

#include "spice_expr/spice_expr.h"

using namespace spice_expr;

class DifferentiatorTest : public ::testing::Test {
 protected:
  ExprArena arena;
  SymbolTable symbols;
  FunctionRegistry functions;
  MockCircuitInterface circuit;

  void SetUp() override {
    circuit.set_node_voltage("1", 2.0);
    circuit.set_node_voltage("2", 1.0);
  }

  double eval(const ExprNode& expr) {
    RealEvaluator evaluator(symbols, functions, circuit);
    return evaluator.evaluate(expr);
  }
};

TEST_F(DifferentiatorTest, ConstantDerivative) {
  auto* expr = arena.make<NumberLiteral>(5.0);
  Differentiator diff(arena, DiffTarget::parameter("x"));
  auto* deriv = diff.differentiate(*expr);
  EXPECT_DOUBLE_EQ(eval(*deriv), 0.0);
}

TEST_F(DifferentiatorTest, VariableDerivative) {
  symbols.define("x", arena.make<NumberLiteral>(3.0));
  auto* expr = arena.make<Identifier>("x");
  Differentiator diff(arena, DiffTarget::parameter("x"));
  auto* deriv = diff.differentiate(*expr);
  EXPECT_DOUBLE_EQ(eval(*deriv), 1.0);
}

TEST_F(DifferentiatorTest, OtherVariableDerivative) {
  symbols.define("y", arena.make<NumberLiteral>(3.0));
  auto* expr = arena.make<Identifier>("y");
  Differentiator diff(arena, DiffTarget::parameter("x"));
  auto* deriv = diff.differentiate(*expr);
  EXPECT_DOUBLE_EQ(eval(*deriv), 0.0);
}

TEST_F(DifferentiatorTest, SumRule) {
  symbols.define("x", arena.make<NumberLiteral>(2.0));
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Add, arena.make<Identifier>("x"),
                                    arena.make<NumberLiteral>(3.0));
  Differentiator diff(arena, DiffTarget::parameter("x"));
  auto* deriv = diff.differentiate(*expr);
  Simplifier simplifier(arena);
  auto* simplified = simplifier.simplify_to_fixpoint(*deriv);
  EXPECT_DOUBLE_EQ(eval(*simplified), 1.0);
}

TEST_F(DifferentiatorTest, DifferenceRule) {
  symbols.define("x", arena.make<NumberLiteral>(2.0));
  auto* expr = arena.make<BinaryOp>(
      BinaryOpType::Subtract,
      arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<NumberLiteral>(2.0),
                           arena.make<Identifier>("x")),
      arena.make<NumberLiteral>(1.0));
  Differentiator diff(arena, DiffTarget::parameter("x"));
  auto* deriv = diff.differentiate(*expr);
  Simplifier simplifier(arena);
  auto* simplified = simplifier.simplify_to_fixpoint(*deriv);
  EXPECT_DOUBLE_EQ(eval(*simplified), 2.0);
}

TEST_F(DifferentiatorTest, ProductRule) {
  symbols.define("x", arena.make<NumberLiteral>(3.0));
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<Identifier>("x"),
                                    arena.make<Identifier>("x"));
  Differentiator diff(arena, DiffTarget::parameter("x"));
  auto* deriv = diff.differentiate(*expr);
  Simplifier simplifier(arena);
  auto* simplified = simplifier.simplify_to_fixpoint(*deriv);
  EXPECT_DOUBLE_EQ(eval(*simplified), 6.0);
}

TEST_F(DifferentiatorTest, PowerRule) {
  symbols.define("x", arena.make<NumberLiteral>(2.0));
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Power, arena.make<Identifier>("x"),
                                    arena.make<NumberLiteral>(3.0));
  Differentiator diff(arena, DiffTarget::parameter("x"));
  auto* deriv = diff.differentiate(*expr);
  Simplifier simplifier(arena);
  auto* simplified = simplifier.simplify_to_fixpoint(*deriv);
  EXPECT_DOUBLE_EQ(eval(*simplified), 12.0);
}

TEST_F(DifferentiatorTest, UnaryNegate) {
  symbols.define("x", arena.make<NumberLiteral>(5.0));
  auto* expr = arena.make<UnaryOp>(UnaryOpType::Negate, arena.make<Identifier>("x"));
  Differentiator diff(arena, DiffTarget::parameter("x"));
  auto* deriv = diff.differentiate(*expr);
  Simplifier simplifier(arena);
  auto* simplified = simplifier.simplify_to_fixpoint(*deriv);
  EXPECT_DOUBLE_EQ(eval(*simplified), -1.0);
}

TEST_F(DifferentiatorTest, SinDerivative) {
  symbols.define("x", arena.make<NumberLiteral>(0.0));
  auto* expr = arena.make<FunctionCall>("sin", std::vector<ExprNode*>{arena.make<Identifier>("x")});
  Differentiator diff(arena, DiffTarget::parameter("x"));
  auto* deriv = diff.differentiate(*expr);
  EXPECT_NEAR(eval(*deriv), 1.0, 1e-10);
}

TEST_F(DifferentiatorTest, CosDerivative) {
  symbols.define("x", arena.make<NumberLiteral>(0.0));
  auto* expr = arena.make<FunctionCall>("cos", std::vector<ExprNode*>{arena.make<Identifier>("x")});
  Differentiator diff(arena, DiffTarget::parameter("x"));
  auto* deriv = diff.differentiate(*expr);
  EXPECT_NEAR(eval(*deriv), 0.0, 1e-10);
}

TEST_F(DifferentiatorTest, ExpDerivative) {
  symbols.define("x", arena.make<NumberLiteral>(0.0));
  auto* expr = arena.make<FunctionCall>("exp", std::vector<ExprNode*>{arena.make<Identifier>("x")});
  Differentiator diff(arena, DiffTarget::parameter("x"));
  auto* deriv = diff.differentiate(*expr);
  EXPECT_NEAR(eval(*deriv), 1.0, 1e-10);
}

TEST_F(DifferentiatorTest, LogDerivative) {
  symbols.define("x", arena.make<NumberLiteral>(2.0));
  auto* expr = arena.make<FunctionCall>("log", std::vector<ExprNode*>{arena.make<Identifier>("x")});
  Differentiator diff(arena, DiffTarget::parameter("x"));
  auto* deriv = diff.differentiate(*expr);
  EXPECT_NEAR(eval(*deriv), 0.5, 1e-10);
}

TEST_F(DifferentiatorTest, SqrtDerivative) {
  symbols.define("x", arena.make<NumberLiteral>(4.0));
  auto* expr =
      arena.make<FunctionCall>("sqrt", std::vector<ExprNode*>{arena.make<Identifier>("x")});
  Differentiator diff(arena, DiffTarget::parameter("x"));
  auto* deriv = diff.differentiate(*expr);
  EXPECT_NEAR(eval(*deriv), 0.25, 1e-10);
}

TEST_F(DifferentiatorTest, NodeVoltageDerivative) {
  auto* expr = arena.make<CircuitNodeRef>("1");
  Differentiator diff(arena, DiffTarget::node_voltage("1"));
  auto* deriv = diff.differentiate(*expr);
  EXPECT_DOUBLE_EQ(eval(*deriv), 1.0);
}

TEST_F(DifferentiatorTest, OtherNodeVoltageDerivative) {
  auto* expr = arena.make<CircuitNodeRef>("1");
  Differentiator diff(arena, DiffTarget::node_voltage("2"));
  auto* deriv = diff.differentiate(*expr);
  EXPECT_DOUBLE_EQ(eval(*deriv), 0.0);
}

TEST_F(DifferentiatorTest, ScaledNodeVoltageDerivative) {
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<NumberLiteral>(2.0),
                                    arena.make<CircuitNodeRef>("1"));
  Differentiator diff(arena, DiffTarget::node_voltage("1"));
  auto* deriv = diff.differentiate(*expr);
  Simplifier simplifier(arena);
  auto* simplified = simplifier.simplify_to_fixpoint(*deriv);
  EXPECT_DOUBLE_EQ(eval(*simplified), 2.0);
}

TEST_F(DifferentiatorTest, DifferentialVoltageDerivative) {
  auto* expr = arena.make<CircuitNodeRef>("1", "2");

  Differentiator diff1(arena, DiffTarget::node_voltage("1"));
  auto* deriv1 = diff1.differentiate(*expr);
  EXPECT_DOUBLE_EQ(eval(*deriv1), 1.0);

  Differentiator diff2(arena, DiffTarget::node_voltage("2"));
  auto* deriv2 = diff2.differentiate(*expr);
  EXPECT_DOUBLE_EQ(eval(*deriv2), -1.0);
}

TEST_F(DifferentiatorTest, MixedExpression) {
  symbols.define("a", arena.make<NumberLiteral>(3.0));
  auto* expr = arena.make<BinaryOp>(
      BinaryOpType::Add, arena.make<Identifier>("a"),
      arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<CircuitNodeRef>("1"),
                           arena.make<CircuitNodeRef>("2")));

  Differentiator diff(arena, DiffTarget::node_voltage("1"));
  auto* deriv = diff.differentiate(*expr);
  Simplifier simplifier(arena);
  auto* simplified = simplifier.simplify_to_fixpoint(*deriv);
  EXPECT_DOUBLE_EQ(eval(*simplified), 1.0);
}

TEST_F(DifferentiatorTest, ChainRule) {
  symbols.define("x", arena.make<NumberLiteral>(0.0));
  auto* expr = arena.make<FunctionCall>(
      "sin",
      std::vector<ExprNode*>{arena.make<BinaryOp>(
          BinaryOpType::Multiply, arena.make<NumberLiteral>(2.0), arena.make<Identifier>("x"))});
  Differentiator diff(arena, DiffTarget::parameter("x"));
  auto* deriv = diff.differentiate(*expr);
  EXPECT_NEAR(eval(*deriv), 2.0, 1e-10);
}

TEST_F(DifferentiatorTest, DiffTargetTypes) {
  auto param = DiffTarget::parameter("x");
  EXPECT_TRUE(param.is_parameter());
  EXPECT_FALSE(param.is_node_voltage());
  EXPECT_FALSE(param.is_device_current());
  EXPECT_EQ(param.name(), "x");

  auto node = DiffTarget::node_voltage("1");
  EXPECT_FALSE(node.is_parameter());
  EXPECT_TRUE(node.is_node_voltage());
  EXPECT_FALSE(node.is_device_current());
  EXPECT_EQ(node.name(), "1");

  auto current = DiffTarget::device_current("R1");
  EXPECT_FALSE(current.is_parameter());
  EXPECT_FALSE(current.is_node_voltage());
  EXPECT_TRUE(current.is_device_current());
  EXPECT_EQ(current.name(), "R1");
}

// Tests for chain rule through parameter dependencies
TEST_F(DifferentiatorTest, ChainRuleThroughParameters) {
  // b = 2*a, c = b + 1 → dc/da = 2
  symbols.define("a", arena.make<NumberLiteral>(3.0));
  symbols.define("b", arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<NumberLiteral>(2.0),
                                           arena.make<Identifier>("a")));
  symbols.define("c", arena.make<BinaryOp>(BinaryOpType::Add, arena.make<Identifier>("b"),
                                           arena.make<NumberLiteral>(1.0)));

  auto* expr = arena.make<Identifier>("c");
  Differentiator diff(arena, DiffTarget::parameter("a"), &symbols);
  auto* deriv = diff.differentiate(*expr);
  Simplifier simplifier(arena, &symbols);
  auto* simplified = simplifier.simplify_to_fixpoint(*deriv);
  EXPECT_DOUBLE_EQ(eval(*simplified), 2.0);
}

TEST_F(DifferentiatorTest, DeepChainRule) {
  // a, b = a^2, c = b + a, d = c * 2
  // dd/da = 2 * (2a + 1) = 4a + 2
  // At a=3: dd/da = 14
  symbols.define("a", arena.make<NumberLiteral>(3.0));
  symbols.define("b", arena.make<BinaryOp>(BinaryOpType::Power, arena.make<Identifier>("a"),
                                           arena.make<NumberLiteral>(2.0)));
  symbols.define("c", arena.make<BinaryOp>(BinaryOpType::Add, arena.make<Identifier>("b"),
                                           arena.make<Identifier>("a")));
  symbols.define("d", arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<Identifier>("c"),
                                           arena.make<NumberLiteral>(2.0)));

  auto* expr = arena.make<Identifier>("d");
  Differentiator diff(arena, DiffTarget::parameter("a"), &symbols);
  auto* deriv = diff.differentiate(*expr);
  Simplifier simplifier(arena, &symbols);
  auto* simplified = simplifier.simplify_to_fixpoint(*deriv);
  EXPECT_DOUBLE_EQ(eval(*simplified), 14.0);
}

TEST_F(DifferentiatorTest, CircularDependencyThrows) {
  // a = b + 1, b = a + 1 (circular)
  symbols.define("a", arena.make<BinaryOp>(BinaryOpType::Add, arena.make<Identifier>("b"),
                                           arena.make<NumberLiteral>(1.0)));
  symbols.define("b", arena.make<BinaryOp>(BinaryOpType::Add, arena.make<Identifier>("a"),
                                           arena.make<NumberLiteral>(1.0)));

  auto* expr = arena.make<Identifier>("a");
  Differentiator diff(arena, DiffTarget::parameter("x"), &symbols);
  EXPECT_THROW(diff.differentiate(*expr), std::runtime_error);
}

TEST_F(DifferentiatorTest, NoChainRuleWithoutSymbols) {
  // Test backward compatibility - without symbol table, chain rule not applied
  symbols.define("a", arena.make<NumberLiteral>(3.0));
  symbols.define("b", arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<NumberLiteral>(2.0),
                                           arena.make<Identifier>("a")));

  auto* expr = arena.make<Identifier>("b");
  Differentiator diff(arena, DiffTarget::parameter("a"));  // No symbols
  auto* deriv = diff.differentiate(*expr);
  EXPECT_DOUBLE_EQ(eval(*deriv), 0.0);  // Returns 0 without chain rule
}

TEST_F(DifferentiatorTest, ChainRuleWithSymbols) {
  // Same as above but with symbol table - chain rule applies
  symbols.define("a", arena.make<NumberLiteral>(3.0));
  symbols.define("b", arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<NumberLiteral>(2.0),
                                           arena.make<Identifier>("a")));

  auto* expr = arena.make<Identifier>("b");
  Differentiator diff(arena, DiffTarget::parameter("a"), &symbols);  // With symbols
  auto* deriv = diff.differentiate(*expr);
  Simplifier simplifier(arena, &symbols);
  auto* simplified = simplifier.simplify_to_fixpoint(*deriv);
  EXPECT_DOUBLE_EQ(eval(*simplified), 2.0);  // Returns 2 with chain rule
}
