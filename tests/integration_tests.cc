#include <gtest/gtest.h>

#include <cmath>

#include "spice_expr/spice_expr.h"

using namespace spice_expr;

class IntegrationTest : public ::testing::Test {
 protected:
  ExpressionEngine engine;
  MockCircuitInterface circuit;

  void SetUp() override {
    circuit.set_node_voltage("1", 1.5);
    circuit.set_node_voltage("2", 0.5);
    circuit.set_node_voltage("out", 0.9);
    circuit.set_device_current("R1", 0.001);
  }
};

TEST_F(IntegrationTest, BasicParameterEvaluation) {
  ExprArena& arena = engine.arena();
  engine.define_parameter("vdd", arena.make<NumberLiteral>(1.8));

  auto* expr = arena.make<Identifier>("vdd");
  EXPECT_DOUBLE_EQ(engine.evaluate_real(*expr), 1.8);
}

TEST_F(IntegrationTest, DependentParameters) {
  ExprArena& arena = engine.arena();
  engine.define_parameter("vdd", arena.make<NumberLiteral>(1.8));
  engine.define_parameter(
      "vth", arena.make<BinaryOp>(BinaryOpType::Subtract, arena.make<Identifier>("vdd"),
                                  arena.make<NumberLiteral>(0.4)));
  engine.define_parameter(
      "vdsat", arena.make<BinaryOp>(BinaryOpType::Subtract, arena.make<Identifier>("vdd"),
                                    arena.make<Identifier>("vth")));

  engine.build_evaluation_order();
  EXPECT_TRUE(engine.has_valid_evaluation_order());
  EXPECT_FALSE(engine.has_cyclic_dependencies());

  EXPECT_DOUBLE_EQ(engine.evaluate_real(*arena.make<Identifier>("vth")), 1.4);
  EXPECT_DOUBLE_EQ(engine.evaluate_real(*arena.make<Identifier>("vdsat")), 0.4);
}

TEST_F(IntegrationTest, UserFunction) {
  ExprArena& arena = engine.arena();

  engine.define_function("square", {"x"},
                         arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<Identifier>("x"),
                                              arena.make<Identifier>("x")));

  auto* expr =
      arena.make<FunctionCall>("square", std::vector<ExprNode*>{arena.make<NumberLiteral>(5.0)});

  EXPECT_DOUBLE_EQ(engine.evaluate_real(*expr), 25.0);
}

TEST_F(IntegrationTest, CircuitValues) {
  ExprArena& arena = engine.arena();

  auto* vdiff = arena.make<CircuitNodeRef>("1", "2");
  EXPECT_DOUBLE_EQ(engine.evaluate_real(*vdiff, circuit), 1.0);

  auto* expr = arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<NumberLiteral>(2.0),
                                    arena.make<CircuitNodeRef>("1"));
  EXPECT_DOUBLE_EQ(engine.evaluate_real(*expr, circuit), 3.0);
}

TEST_F(IntegrationTest, Simplification) {
  ExprArena& arena = engine.arena();

  auto* expr = arena.make<BinaryOp>(
      BinaryOpType::Add,
      arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<NumberLiteral>(2.0),
                           arena.make<NumberLiteral>(3.0)),
      arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<Identifier>("x"),
                           arena.make<NumberLiteral>(0.0)));

  auto* simplified = engine.simplify_to_fixpoint(*expr);
  ASSERT_EQ(simplified->type(), NodeType::NumberLiteral);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(simplified)->real(), 6.0);
}

TEST_F(IntegrationTest, Differentiation) {
  ExprArena& arena = engine.arena();

  auto* expr = arena.make<BinaryOp>(
      BinaryOpType::Add,
      arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<NumberLiteral>(2.0),
                           arena.make<CircuitNodeRef>("1")),
      arena.make<NumberLiteral>(3.0));

  auto* deriv = engine.differentiate(*expr, DiffTarget::node_voltage("1"));
  EXPECT_DOUBLE_EQ(engine.evaluate_real(*deriv, circuit), 2.0);
}

TEST_F(IntegrationTest, ParseAndEvaluate) {
  ExprArena& arena = engine.arena();
  engine.define_parameter("vdd", arena.make<NumberLiteral>(1.8));

  auto* expr = parse_expression("vdd * 2 + 0.4", arena);
  EXPECT_DOUBLE_EQ(engine.evaluate_real(*expr), 4.0);
}

TEST_F(IntegrationTest, ComplexExpression) {
  ExprArena& arena = engine.arena();

  auto* expr = parse_expression("sin(0)**2 + cos(0)**2", arena);
  EXPECT_NEAR(engine.evaluate_real(*expr), 1.0, 1e-10);
}

TEST_F(IntegrationTest, ParameterOverride) {
  ExprArena& arena = engine.arena();
  engine.define_parameter("vdd", arena.make<NumberLiteral>(1.8));

  EXPECT_DOUBLE_EQ(engine.evaluate_real(*arena.make<Identifier>("vdd")), 1.8);

  engine.set_parameter_override("vdd", arena.make<NumberLiteral>(3.3));
  EXPECT_DOUBLE_EQ(engine.evaluate_real(*arena.make<Identifier>("vdd")), 3.3);

  engine.clear_parameter_override("vdd");
  EXPECT_DOUBLE_EQ(engine.evaluate_real(*arena.make<Identifier>("vdd")), 1.8);
}

TEST_F(IntegrationTest, SubcircuitScope) {
  ExprArena& arena = engine.arena();
  engine.define_parameter("vdd", arena.make<NumberLiteral>(1.8));

  auto child = engine.create_subcircuit_scope();
  child->define("local_param", arena.make<NumberLiteral>(0.5));

  EXPECT_TRUE(child->exists("vdd"));
  EXPECT_TRUE(child->exists("local_param"));
  EXPECT_FALSE(engine.symbols().exists("local_param"));
}

TEST_F(IntegrationTest, CyclicDependencyDetection) {
  ExprArena& arena = engine.arena();
  engine.define_parameter("a", arena.make<Identifier>("b"));
  engine.define_parameter("b", arena.make<Identifier>("a"));

  engine.build_evaluation_order();
  EXPECT_FALSE(engine.has_valid_evaluation_order());
  EXPECT_TRUE(engine.has_cyclic_dependencies());

  auto cycle = engine.get_cyclic_dependency_path();
  EXPECT_GE(cycle.size(), 2);
}

TEST_F(IntegrationTest, EngineeringNotation) {
  ExprArena& arena = engine.arena();

  auto* expr = parse_expression("1k + 500", arena);
  EXPECT_DOUBLE_EQ(engine.evaluate_real(*expr), 1500.0);

  auto* expr2 = parse_expression("10n * 1meg", arena);
  EXPECT_DOUBLE_EQ(engine.evaluate_real(*expr2), 0.01);
}

TEST_F(IntegrationTest, TernaryInExpression) {
  ExprArena& arena = engine.arena();
  engine.define_parameter("mode", arena.make<NumberLiteral>(1.0));

  auto* expr = parse_expression("mode > 0 ? 100 : 200", arena);
  EXPECT_DOUBLE_EQ(engine.evaluate_real(*expr), 100.0);

  engine.set_parameter_override("mode", arena.make<NumberLiteral>(0.0));
  EXPECT_DOUBLE_EQ(engine.evaluate_real(*expr), 200.0);
}

TEST_F(IntegrationTest, NestedFunctions) {
  ExprArena& arena = engine.arena();

  auto* expr = parse_expression("sqrt(pow(3, 2) + pow(4, 2))", arena);
  EXPECT_DOUBLE_EQ(engine.evaluate_real(*expr), 5.0);
}

TEST_F(IntegrationTest, PrintExpression) {
  ExprArena& arena = engine.arena();

  auto* expr = arena.make<BinaryOp>(
      BinaryOpType::Add,
      arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<NumberLiteral>(2.0),
                           arena.make<Identifier>("x")),
      arena.make<NumberLiteral>(3.0));

  ExprPrinter printer(PrintFormat::SPICE);
  std::string result = printer.print(*expr);
  EXPECT_FALSE(result.empty());
}

TEST_F(IntegrationTest, EvaluateAllParameters) {
  ExprArena& arena = engine.arena();
  engine.define_parameter("a", arena.make<NumberLiteral>(1.0));
  engine.define_parameter("b", arena.make<BinaryOp>(BinaryOpType::Add, arena.make<Identifier>("a"),
                                                    arena.make<NumberLiteral>(1.0)));
  engine.define_parameter("c",
                          arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<Identifier>("b"),
                                               arena.make<NumberLiteral>(2.0)));

  // evaluate_all_parameters verifies all expressions can be evaluated without error
  engine.evaluate_all_parameters(circuit);

  // Verify final values through normal evaluation
  auto* cExpr = engine.symbols().lookup("c")->expression;
  EXPECT_DOUBLE_EQ(engine.evaluate_real(*cExpr), 4.0);
}

TEST_F(IntegrationTest, ComplexNumberEvaluation) {
  ExprArena& arena = engine.arena();

  auto* expr = arena.make<BinaryOp>(BinaryOpType::Add, arena.make<NumberLiteral>(1.0, 2.0),
                                    arena.make<NumberLiteral>(3.0, 4.0));

  auto result = engine.evaluate_complex(*expr);
  EXPECT_DOUBLE_EQ(result.real(), 4.0);
  EXPECT_DOUBLE_EQ(result.imag(), 6.0);
}

TEST_F(IntegrationTest, FullSPICEExpression) {
  ExprArena& arena = engine.arena();

  engine.define_parameter("vdd", arena.make<NumberLiteral>(1.8));
  engine.define_parameter("vth", arena.make<NumberLiteral>(0.4));
  engine.define_parameter("kp", arena.make<NumberLiteral>(100e-6));
  engine.define_parameter("w", arena.make<NumberLiteral>(10e-6));
  engine.define_parameter("l", arena.make<NumberLiteral>(1e-6));

  engine.define_function(
      "ids_sat", {"vgs", "vth", "kp", "w", "l"},
      arena.make<BinaryOp>(
          BinaryOpType::Multiply,
          arena.make<BinaryOp>(
              BinaryOpType::Multiply,
              arena.make<BinaryOp>(BinaryOpType::Divide, arena.make<Identifier>("kp"),
                                   arena.make<NumberLiteral>(2.0)),
              arena.make<BinaryOp>(BinaryOpType::Divide, arena.make<Identifier>("w"),
                                   arena.make<Identifier>("l"))),
          arena.make<BinaryOp>(
              BinaryOpType::Power,
              arena.make<BinaryOp>(BinaryOpType::Subtract, arena.make<Identifier>("vgs"),
                                   arena.make<Identifier>("vth")),
              arena.make<NumberLiteral>(2.0))));

  engine.build_evaluation_order();

  circuit.set_node_voltage("g", 1.0);

  auto* expr = arena.make<FunctionCall>(
      "ids_sat", std::vector<ExprNode*>{arena.make<CircuitNodeRef>("g"),
                                        arena.make<Identifier>("vth"), arena.make<Identifier>("kp"),
                                        arena.make<Identifier>("w"), arena.make<Identifier>("l")});

  double ids = engine.evaluate_real(*expr, circuit);
  double expected = (100e-6 / 2.0) * (10e-6 / 1e-6) * std::pow(1.0 - 0.4, 2);
  EXPECT_NEAR(ids, expected, 1e-15);
}

TEST_F(IntegrationTest, ParameterFlagging) {
  ExprArena& arena = engine.arena();

  engine.define_parameter("a", arena.make<NumberLiteral>(1.0));
  engine.define_parameter("b", arena.make<NumberLiteral>(2.0));

  engine.set_parameter_flag("a", "optimize");
  EXPECT_TRUE(engine.has_parameter_flag("a", "optimize"));
  EXPECT_FALSE(engine.has_parameter_flag("b", "optimize"));

  engine.clear_parameter_flag("a", "optimize");
  EXPECT_FALSE(engine.has_parameter_flag("a", "optimize"));
}

TEST_F(IntegrationTest, GetParametersWithFlag) {
  ExprArena& arena = engine.arena();

  auto* exprA = arena.make<NumberLiteral>(1.0);
  auto* exprB = arena.make<NumberLiteral>(2.0);
  auto* exprC = arena.make<NumberLiteral>(3.0);
  engine.define_parameter("a", exprA);
  engine.define_parameter("b", exprB);
  engine.define_parameter("c", exprC);

  engine.set_parameter_flag("a", "optimize");
  engine.set_parameter_flag("c", "optimize");

  auto params = engine.get_parameters_with_flag("optimize");
  EXPECT_EQ(params.size(), 2);
  EXPECT_TRUE(std::find(params.begin(), params.end(), exprA) != params.end());
  EXPECT_TRUE(std::find(params.begin(), params.end(), exprC) != params.end());
}

TEST_F(IntegrationTest, AffectedParameters) {
  ExprArena& arena = engine.arena();

  // a -> b -> c dependency chain
  auto* exprA = arena.make<NumberLiteral>(1.0);
  auto* exprB = arena.make<BinaryOp>(BinaryOpType::Add, arena.make<Identifier>("a"),
                                     arena.make<NumberLiteral>(1.0));
  auto* exprC = arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<Identifier>("b"),
                                     arena.make<NumberLiteral>(2.0));
  engine.define_parameter("a", exprA);
  engine.define_parameter("b", exprB);
  engine.define_parameter("c", exprC);

  engine.build_evaluation_order();

  // Query affected by expression containing "a"
  auto affected = engine.get_affected_parameters(*arena.make<Identifier>("a"));
  EXPECT_EQ(affected.size(), 2);
  EXPECT_TRUE(affected.count(exprB) > 0);
  EXPECT_TRUE(affected.count(exprC) > 0);

  // Query affected by expression containing "b"
  auto affectedB = engine.get_affected_parameters(*arena.make<Identifier>("b"));
  EXPECT_EQ(affectedB.size(), 1);
  EXPECT_TRUE(affectedB.count(exprC) > 0);
}

TEST_F(IntegrationTest, AffectedParametersByFlag) {
  ExprArena& arena = engine.arena();

  // a -> b -> c dependency chain
  auto* exprA = arena.make<NumberLiteral>(1.0);
  auto* exprB = arena.make<BinaryOp>(BinaryOpType::Add, arena.make<Identifier>("a"),
                                     arena.make<NumberLiteral>(1.0));
  auto* exprC = arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<Identifier>("b"),
                                     arena.make<NumberLiteral>(2.0));
  engine.define_parameter("a", exprA);
  engine.define_parameter("b", exprB);
  engine.define_parameter("c", exprC);

  engine.build_evaluation_order();
  engine.set_parameter_flag("a", "optimize");

  // Should return a (flagged) plus b and c (affected)
  auto affected = engine.get_affected_parameters_by_flag("optimize");
  EXPECT_EQ(affected.size(), 3);
  EXPECT_TRUE(affected.count(exprA) > 0);
  EXPECT_TRUE(affected.count(exprB) > 0);
  EXPECT_TRUE(affected.count(exprC) > 0);
}

TEST_F(IntegrationTest, AffectedParametersByFlagMultipleFlagged) {
  ExprArena& arena = engine.arena();

  // Two separate chains: a -> b, c -> d
  auto* exprA = arena.make<NumberLiteral>(1.0);
  auto* exprB = arena.make<Identifier>("a");
  auto* exprC = arena.make<NumberLiteral>(2.0);
  auto* exprD = arena.make<Identifier>("c");
  engine.define_parameter("a", exprA);
  engine.define_parameter("b", exprB);
  engine.define_parameter("c", exprC);
  engine.define_parameter("d", exprD);

  engine.build_evaluation_order();
  engine.set_parameter_flag("a", "sweep");
  engine.set_parameter_flag("c", "sweep");

  auto affected = engine.get_affected_parameters_by_flag("sweep");
  EXPECT_EQ(affected.size(), 4);
  EXPECT_TRUE(affected.count(exprA) > 0);
  EXPECT_TRUE(affected.count(exprB) > 0);
  EXPECT_TRUE(affected.count(exprC) > 0);
  EXPECT_TRUE(affected.count(exprD) > 0);
}
