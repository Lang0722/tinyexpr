#include <gtest/gtest.h>

#include <cmath>

#include "spice_expr/spice_expr.h"

using namespace spice_expr;

class FunctionTest : public ::testing::Test {
 protected:
  ExprArena arena;
  SymbolTable symbols;
  FunctionRegistry functions;
  MockCircuitInterface circuit;
};

TEST_F(FunctionTest, BuiltinSin) {
  double result = functions.evaluate_real_builtin("sin", {M_PI / 2});
  EXPECT_NEAR(result, 1.0, 1e-10);
}

TEST_F(FunctionTest, BuiltinCos) {
  double result = functions.evaluate_real_builtin("cos", {0.0});
  EXPECT_NEAR(result, 1.0, 1e-10);
}

TEST_F(FunctionTest, BuiltinTan) {
  double result = functions.evaluate_real_builtin("tan", {M_PI / 4});
  EXPECT_NEAR(result, 1.0, 1e-10);
}

TEST_F(FunctionTest, BuiltinExp) {
  double result = functions.evaluate_real_builtin("exp", {0.0});
  EXPECT_DOUBLE_EQ(result, 1.0);

  double result2 = functions.evaluate_real_builtin("exp", {1.0});
  EXPECT_NEAR(result2, M_E, 1e-10);
}

TEST_F(FunctionTest, BuiltinLog) {
  double result = functions.evaluate_real_builtin("log", {M_E});
  EXPECT_NEAR(result, 1.0, 1e-10);

  double result2 = functions.evaluate_real_builtin("ln", {M_E});
  EXPECT_NEAR(result2, 1.0, 1e-10);
}

TEST_F(FunctionTest, BuiltinLog10) {
  double result = functions.evaluate_real_builtin("log10", {100.0});
  EXPECT_NEAR(result, 2.0, 1e-10);
}

TEST_F(FunctionTest, BuiltinSqrt) {
  double result = functions.evaluate_real_builtin("sqrt", {16.0});
  EXPECT_DOUBLE_EQ(result, 4.0);
}

TEST_F(FunctionTest, BuiltinAbs) {
  double result = functions.evaluate_real_builtin("abs", {-5.0});
  EXPECT_DOUBLE_EQ(result, 5.0);
}

TEST_F(FunctionTest, BuiltinPow) {
  double result = functions.evaluate_real_builtin("pow", {2.0, 3.0});
  EXPECT_DOUBLE_EQ(result, 8.0);
}

TEST_F(FunctionTest, BuiltinPwr) {
  double result = functions.evaluate_real_builtin("pwr", {-2.0, 3.0});
  EXPECT_DOUBLE_EQ(result, 8.0);
}

TEST_F(FunctionTest, BuiltinPwrs) {
  double result = functions.evaluate_real_builtin("pwrs", {-2.0, 3.0});
  EXPECT_DOUBLE_EQ(result, -8.0);
}

TEST_F(FunctionTest, BuiltinMin) {
  double result = functions.evaluate_real_builtin("min", {5.0, 2.0, 8.0, 1.0});
  EXPECT_DOUBLE_EQ(result, 1.0);
}

TEST_F(FunctionTest, BuiltinMax) {
  double result = functions.evaluate_real_builtin("max", {5.0, 2.0, 8.0, 1.0});
  EXPECT_DOUBLE_EQ(result, 8.0);
}

TEST_F(FunctionTest, BuiltinLimit) {
  double result = functions.evaluate_real_builtin("limit", {5.0, 0.0, 3.0});
  EXPECT_DOUBLE_EQ(result, 3.0);

  double result2 = functions.evaluate_real_builtin("limit", {-1.0, 0.0, 3.0});
  EXPECT_DOUBLE_EQ(result2, 0.0);

  double result3 = functions.evaluate_real_builtin("limit", {2.0, 0.0, 3.0});
  EXPECT_DOUBLE_EQ(result3, 2.0);
}

TEST_F(FunctionTest, BuiltinFloor) {
  double result = functions.evaluate_real_builtin("floor", {3.7});
  EXPECT_DOUBLE_EQ(result, 3.0);
}

TEST_F(FunctionTest, BuiltinCeil) {
  double result = functions.evaluate_real_builtin("ceil", {3.2});
  EXPECT_DOUBLE_EQ(result, 4.0);
}

TEST_F(FunctionTest, BuiltinSgn) {
  EXPECT_DOUBLE_EQ(functions.evaluate_real_builtin("sgn", {5.0}), 1.0);
  EXPECT_DOUBLE_EQ(functions.evaluate_real_builtin("sgn", {-5.0}), -1.0);
  EXPECT_DOUBLE_EQ(functions.evaluate_real_builtin("sgn", {0.0}), 0.0);
}

TEST_F(FunctionTest, BuiltinDb) {
  double result = functions.evaluate_real_builtin("db", {10.0});
  EXPECT_NEAR(result, 20.0, 1e-10);
}

TEST_F(FunctionTest, BuiltinHypot) {
  double result = functions.evaluate_real_builtin("hypot", {3.0, 4.0});
  EXPECT_DOUBLE_EQ(result, 5.0);
}

TEST_F(FunctionTest, BuiltinAtan2) {
  double result = functions.evaluate_real_builtin("atan2", {1.0, 1.0});
  EXPECT_NEAR(result, M_PI / 4, 1e-10);
}

TEST_F(FunctionTest, ComplexSin) {
  auto result = functions.evaluate_complex_builtin("sin", {{0.0, 0.0}});
  EXPECT_NEAR(result.real(), 0.0, 1e-10);
  EXPECT_NEAR(result.imag(), 0.0, 1e-10);
}

TEST_F(FunctionTest, ComplexExp) {
  auto result = functions.evaluate_complex_builtin("exp", {{0.0, M_PI}});
  EXPECT_NEAR(result.real(), -1.0, 1e-10);
  EXPECT_NEAR(result.imag(), 0.0, 1e-10);
}

TEST_F(FunctionTest, ComplexReal) {
  auto result = functions.evaluate_complex_builtin("real", {{3.0, 4.0}});
  EXPECT_DOUBLE_EQ(result.real(), 3.0);
}

TEST_F(FunctionTest, ComplexImag) {
  auto result = functions.evaluate_complex_builtin("imag", {{3.0, 4.0}});
  EXPECT_DOUBLE_EQ(result.real(), 4.0);
}

TEST_F(FunctionTest, ComplexMag) {
  auto result = functions.evaluate_complex_builtin("mag", {{3.0, 4.0}});
  EXPECT_DOUBLE_EQ(result.real(), 5.0);
}

TEST_F(FunctionTest, ComplexPhase) {
  auto result = functions.evaluate_complex_builtin("phase", {{1.0, 1.0}});
  EXPECT_NEAR(result.real(), M_PI / 4, 1e-10);
}

TEST_F(FunctionTest, ComplexConj) {
  auto result = functions.evaluate_complex_builtin("conj", {{3.0, 4.0}});
  EXPECT_DOUBLE_EQ(result.real(), 3.0);
  EXPECT_DOUBLE_EQ(result.imag(), -4.0);
}

TEST_F(FunctionTest, CaseInsensitive) {
  EXPECT_NE(functions.lookup_builtin("SIN"), nullptr);
  EXPECT_NE(functions.lookup_builtin("Sin"), nullptr);
  EXPECT_NE(functions.lookup_builtin("sin"), nullptr);
}

TEST_F(FunctionTest, UserFunction) {
  auto* body = arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<Identifier>("x"),
                                    arena.make<Identifier>("x"));

  functions.register_user_function(
      std::make_unique<UserFunction>("square", std::vector<std::string>{"x"}, body));

  EXPECT_TRUE(functions.exists("square"));
  EXPECT_TRUE(functions.is_user_defined("square"));
  EXPECT_FALSE(functions.is_builtin("square"));

  const UserFunction* func = functions.lookup_user("square");
  ASSERT_NE(func, nullptr);
  EXPECT_EQ(func->name(), "square");
  EXPECT_EQ(func->parameter_count(), 1);
}

TEST_F(FunctionTest, Unregister_user_function) {
  auto* body = arena.make<NumberLiteral>(42.0);
  functions.register_user_function(
      std::make_unique<UserFunction>("test", std::vector<std::string>{}, body));

  EXPECT_TRUE(functions.exists("test"));
  functions.unregister_user_function("test");
  EXPECT_FALSE(functions.exists("test"));
}

TEST_F(FunctionTest, AllFunctionNames) {
  auto names = functions.all_function_names();
  EXPECT_GT(names.size(), 0);

  auto hasFunction = [&](const std::string& name) {
    return std::find(names.begin(), names.end(), name) != names.end();
  };

  EXPECT_TRUE(hasFunction("sin"));
  EXPECT_TRUE(hasFunction("cos"));
  EXPECT_TRUE(hasFunction("sqrt"));
}

TEST_F(FunctionTest, UnknownFunctionThrows) {
  EXPECT_THROW(functions.evaluate_real_builtin("nonexistent", {1.0}), std::runtime_error);
}

TEST_F(FunctionTest, TooFewArgumentsThrows) {
  EXPECT_THROW(functions.evaluate_real_builtin("pow", {2.0}), std::runtime_error);
}

TEST_F(FunctionTest, TooManyArgumentsThrows) {
  EXPECT_THROW(functions.evaluate_real_builtin("sin", {1.0, 2.0}), std::runtime_error);
}

TEST_F(FunctionTest, LogNegativeThrows) {
  EXPECT_THROW(functions.evaluate_real_builtin("log", {-1.0}), std::runtime_error);
}

TEST_F(FunctionTest, SqrtNegativeThrows) {
  EXPECT_THROW(functions.evaluate_real_builtin("sqrt", {-1.0}), std::runtime_error);
}
