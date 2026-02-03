#include <gtest/gtest.h>

#include <cmath>

#include "spice_expr/spice_expr.h"

using namespace spice_expr;

class EvaluatorTest : public ::testing::Test {
 protected:
  ExprArena arena;
  SymbolTable symbols;
  FunctionRegistry functions;
  MockCircuitInterface circuit;

  void SetUp() override {
    circuit.set_node_voltage("1", 1.5);
    circuit.set_node_voltage("2", 0.5);
    circuit.set_node_voltage("out", 0.9);
    circuit.set_device_current("R1", 0.001);
  }
};

TEST_F(EvaluatorTest, NumberLiteral) {
  auto* expr = arena.make<NumberLiteral>(42.0);
  RealEvaluator eval(symbols, functions, circuit);
  EXPECT_DOUBLE_EQ(eval.evaluate(*expr), 42.0);
}

TEST_F(EvaluatorTest, BinaryAddition) {
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Add, arena.make<NumberLiteral>(2.0),
                                    arena.make<NumberLiteral>(3.0));
  RealEvaluator eval(symbols, functions, circuit);
  EXPECT_DOUBLE_EQ(eval.evaluate(*expr), 5.0);
}

TEST_F(EvaluatorTest, BinarySubtraction) {
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Subtract, arena.make<NumberLiteral>(5.0),
                                    arena.make<NumberLiteral>(3.0));
  RealEvaluator eval(symbols, functions, circuit);
  EXPECT_DOUBLE_EQ(eval.evaluate(*expr), 2.0);
}

TEST_F(EvaluatorTest, BinaryMultiplication) {
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<NumberLiteral>(4.0),
                                    arena.make<NumberLiteral>(3.0));
  RealEvaluator eval(symbols, functions, circuit);
  EXPECT_DOUBLE_EQ(eval.evaluate(*expr), 12.0);
}

TEST_F(EvaluatorTest, BinaryDivision) {
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Divide, arena.make<NumberLiteral>(10.0),
                                    arena.make<NumberLiteral>(4.0));
  RealEvaluator eval(symbols, functions, circuit);
  EXPECT_DOUBLE_EQ(eval.evaluate(*expr), 2.5);
}

TEST_F(EvaluatorTest, BinaryPower) {
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Power, arena.make<NumberLiteral>(2.0),
                                    arena.make<NumberLiteral>(3.0));
  RealEvaluator eval(symbols, functions, circuit);
  EXPECT_DOUBLE_EQ(eval.evaluate(*expr), 8.0);
}

TEST_F(EvaluatorTest, UnaryNegate) {
  auto* expr = arena.make<UnaryOp>(UnaryOpType::Negate, arena.make<NumberLiteral>(5.0));
  RealEvaluator eval(symbols, functions, circuit);
  EXPECT_DOUBLE_EQ(eval.evaluate(*expr), -5.0);
}

TEST_F(EvaluatorTest, ComparisonOperators) {
  RealEvaluator eval(symbols, functions, circuit);

  auto* less = arena.make<BinaryOp>(BinaryOpType::Less, arena.make<NumberLiteral>(1.0),
                                    arena.make<NumberLiteral>(2.0));
  EXPECT_DOUBLE_EQ(eval.evaluate(*less), 1.0);

  auto* greater = arena.make<BinaryOp>(BinaryOpType::Greater, arena.make<NumberLiteral>(1.0),
                                       arena.make<NumberLiteral>(2.0));
  EXPECT_DOUBLE_EQ(eval.evaluate(*greater), 0.0);

  auto* eq = arena.make<BinaryOp>(BinaryOpType::Equal, arena.make<NumberLiteral>(2.0),
                                  arena.make<NumberLiteral>(2.0));
  EXPECT_DOUBLE_EQ(eval.evaluate(*eq), 1.0);
}

TEST_F(EvaluatorTest, LogicalOperators) {
  RealEvaluator eval(symbols, functions, circuit);

  auto* andOp = arena.make<BinaryOp>(BinaryOpType::LogicalAnd, arena.make<NumberLiteral>(1.0),
                                     arena.make<NumberLiteral>(1.0));
  EXPECT_DOUBLE_EQ(eval.evaluate(*andOp), 1.0);

  auto* orOp = arena.make<BinaryOp>(BinaryOpType::LogicalOr, arena.make<NumberLiteral>(0.0),
                                    arena.make<NumberLiteral>(1.0));
  EXPECT_DOUBLE_EQ(eval.evaluate(*orOp), 1.0);

  auto* notOp = arena.make<UnaryOp>(UnaryOpType::LogicalNot, arena.make<NumberLiteral>(0.0));
  EXPECT_DOUBLE_EQ(eval.evaluate(*notOp), 1.0);
}

TEST_F(EvaluatorTest, Parameter) {
  symbols.define("vdd", arena.make<NumberLiteral>(1.8));
  auto* expr = arena.make<Identifier>("vdd");
  RealEvaluator eval(symbols, functions, circuit);
  EXPECT_DOUBLE_EQ(eval.evaluate(*expr), 1.8);
}

TEST_F(EvaluatorTest, ParameterExpression) {
  symbols.define("vdd", arena.make<NumberLiteral>(1.8));
  symbols.define("vth", arena.make<BinaryOp>(BinaryOpType::Subtract, arena.make<Identifier>("vdd"),
                                             arena.make<NumberLiteral>(0.4)));

  auto* expr = arena.make<Identifier>("vth");
  RealEvaluator eval(symbols, functions, circuit);
  EXPECT_DOUBLE_EQ(eval.evaluate(*expr), 1.4);
}

TEST_F(EvaluatorTest, CircuitNodeVoltage) {
  auto* expr = arena.make<CircuitNodeRef>("1");
  RealEvaluator eval(symbols, functions, circuit);
  EXPECT_DOUBLE_EQ(eval.evaluate(*expr), 1.5);
}

TEST_F(EvaluatorTest, CircuitDifferentialVoltage) {
  auto* expr = arena.make<CircuitNodeRef>("1", "2");
  RealEvaluator eval(symbols, functions, circuit);
  EXPECT_DOUBLE_EQ(eval.evaluate(*expr), 1.0);
}

TEST_F(EvaluatorTest, CircuitDeviceCurrent) {
  auto* expr = arena.make<CircuitCurrentRef>("R1");
  RealEvaluator eval(symbols, functions, circuit);
  EXPECT_DOUBLE_EQ(eval.evaluate(*expr), 0.001);
}

TEST_F(EvaluatorTest, BuiltinFunctions) {
  RealEvaluator eval(symbols, functions, circuit);

  auto* sinExpr =
      arena.make<FunctionCall>("sin", std::vector<ExprNode*>{arena.make<NumberLiteral>(0.0)});
  EXPECT_NEAR(eval.evaluate(*sinExpr), 0.0, 1e-10);

  auto* cosExpr =
      arena.make<FunctionCall>("cos", std::vector<ExprNode*>{arena.make<NumberLiteral>(0.0)});
  EXPECT_NEAR(eval.evaluate(*cosExpr), 1.0, 1e-10);

  auto* sqrtExpr =
      arena.make<FunctionCall>("sqrt", std::vector<ExprNode*>{arena.make<NumberLiteral>(4.0)});
  EXPECT_DOUBLE_EQ(eval.evaluate(*sqrtExpr), 2.0);

  auto* absExpr =
      arena.make<FunctionCall>("abs", std::vector<ExprNode*>{arena.make<NumberLiteral>(-5.0)});
  EXPECT_DOUBLE_EQ(eval.evaluate(*absExpr), 5.0);
}

TEST_F(EvaluatorTest, MinMaxFunctions) {
  RealEvaluator eval(symbols, functions, circuit);

  auto* minExpr = arena.make<FunctionCall>(
      "min", std::vector<ExprNode*>{arena.make<NumberLiteral>(3.0), arena.make<NumberLiteral>(1.0),
                                    arena.make<NumberLiteral>(2.0)});
  EXPECT_DOUBLE_EQ(eval.evaluate(*minExpr), 1.0);

  auto* maxExpr = arena.make<FunctionCall>(
      "max", std::vector<ExprNode*>{arena.make<NumberLiteral>(3.0), arena.make<NumberLiteral>(1.0),
                                    arena.make<NumberLiteral>(2.0)});
  EXPECT_DOUBLE_EQ(eval.evaluate(*maxExpr), 3.0);
}

TEST_F(EvaluatorTest, TernaryConditional) {
  RealEvaluator eval(symbols, functions, circuit);

  auto* ternTrue = arena.make<TernaryConditional>(arena.make<NumberLiteral>(1.0),
                                                  arena.make<NumberLiteral>(10.0),
                                                  arena.make<NumberLiteral>(20.0));
  EXPECT_DOUBLE_EQ(eval.evaluate(*ternTrue), 10.0);

  auto* ternFalse = arena.make<TernaryConditional>(arena.make<NumberLiteral>(0.0),
                                                   arena.make<NumberLiteral>(10.0),
                                                   arena.make<NumberLiteral>(20.0));
  EXPECT_DOUBLE_EQ(eval.evaluate(*ternFalse), 20.0);
}

TEST_F(EvaluatorTest, ComplexNumberLiteral) {
  auto* expr = arena.make<NumberLiteral>(3.0, 4.0);
  ComplexEvaluator eval(symbols, functions, circuit);
  auto result = eval.evaluate(*expr);
  EXPECT_DOUBLE_EQ(result.real(), 3.0);
  EXPECT_DOUBLE_EQ(result.imag(), 4.0);
}

TEST_F(EvaluatorTest, ComplexAddition) {
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Add, arena.make<NumberLiteral>(1.0, 2.0),
                                    arena.make<NumberLiteral>(3.0, 4.0));
  ComplexEvaluator eval(symbols, functions, circuit);
  auto result = eval.evaluate(*expr);
  EXPECT_DOUBLE_EQ(result.real(), 4.0);
  EXPECT_DOUBLE_EQ(result.imag(), 6.0);
}

TEST_F(EvaluatorTest, ComplexMultiplication) {
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<NumberLiteral>(1.0, 2.0),
                                    arena.make<NumberLiteral>(3.0, 4.0));
  ComplexEvaluator eval(symbols, functions, circuit);
  auto result = eval.evaluate(*expr);
  EXPECT_DOUBLE_EQ(result.real(), -5.0);
  EXPECT_DOUBLE_EQ(result.imag(), 10.0);
}

TEST_F(EvaluatorTest, RequiresComplexEvaluation) {
  auto* realExpr = arena.make<BinaryOp>(BinaryOpType::Add, arena.make<NumberLiteral>(1.0),
                                        arena.make<NumberLiteral>(2.0));
  EXPECT_FALSE(requires_complex_evaluation(*realExpr));

  auto* complexExpr = arena.make<BinaryOp>(BinaryOpType::Add, arena.make<NumberLiteral>(1.0, 2.0),
                                           arena.make<NumberLiteral>(3.0));
  EXPECT_TRUE(requires_complex_evaluation(*complexExpr));
}

TEST_F(EvaluatorTest, DivisionByZeroThrows) {
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Divide, arena.make<NumberLiteral>(1.0),
                                    arena.make<NumberLiteral>(0.0));
  RealEvaluator eval(symbols, functions, circuit);
  EXPECT_THROW(eval.evaluate(*expr), EvaluationError);
}

TEST_F(EvaluatorTest, UndefinedParameterThrows) {
  auto* expr = arena.make<Identifier>("undefined");
  RealEvaluator eval(symbols, functions, circuit);
  EXPECT_THROW(eval.evaluate(*expr), EvaluationError);
}

TEST_F(EvaluatorTest, NestedExpression) {
  auto* expr = arena.make<BinaryOp>(
      BinaryOpType::Add,
      arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<NumberLiteral>(2.0),
                           arena.make<NumberLiteral>(3.0)),
      arena.make<BinaryOp>(BinaryOpType::Divide, arena.make<NumberLiteral>(8.0),
                           arena.make<NumberLiteral>(2.0)));
  RealEvaluator eval(symbols, functions, circuit);
  EXPECT_DOUBLE_EQ(eval.evaluate(*expr), 10.0);
}
