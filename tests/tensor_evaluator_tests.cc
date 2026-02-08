#include <gtest/gtest.h>

#include <cmath>
#include <complex>
#include <vector>

#include "spice_expr/array/xtensor.h"
#include "spice_expr/ast/arena.h"
#include "spice_expr/circuit/circuit_interface.h"
#include "spice_expr/function/function_registry.h"
#include "spice_expr/parser/parser.h"
#include "spice_expr/symbol/symbol_table.h"
#include "spice_expr/visitor/evaluator.h"
#include "spice_expr/visitor/tensor_evaluator.h"

namespace spice_expr {
namespace {

class TensorEvaluatorTest : public ::testing::Test {
 protected:
  void SetUp() override {
    arena_ = std::make_unique<ExprArena>();
    symbols_ = std::make_unique<SymbolTable>();
    functions_ = std::make_unique<FunctionRegistry>();
    circuit_ = std::make_unique<NullCircuitInterface>();
  }

  XTensor evaluate(const std::string& expr_str) {
    ExprNode* expr = parse_expression(expr_str, *arena_);
    TensorEvaluator evaluator(*symbols_, *functions_, *circuit_, arena_.get());
    return evaluator.evaluate(*expr);
  }

  double evaluate_real(const std::string& expr_str) {
    XTensor val = evaluate(expr_str);
    EXPECT_TRUE(val.is_scalar()) << "Expected scalar result";
    return val.get_real_at(0);
  }

  XTensor evaluate_array(const std::string& expr_str) {
    XTensor val = evaluate(expr_str);
    EXPECT_FALSE(val.is_scalar()) << "Expected array result";
    return val;
  }

  std::unique_ptr<ExprArena> arena_;
  std::unique_ptr<SymbolTable> symbols_;
  std::unique_ptr<FunctionRegistry> functions_;
  std::unique_ptr<NullCircuitInterface> circuit_;
};

// Basic scalar evaluation (should still work)

TEST_F(TensorEvaluatorTest, ScalarLiteral) {
  double result = evaluate_real("42.0");
  EXPECT_DOUBLE_EQ(result, 42.0);
}

TEST_F(TensorEvaluatorTest, ScalarArithmetic) {
  EXPECT_DOUBLE_EQ(evaluate_real("2 + 3"), 5.0);
  EXPECT_DOUBLE_EQ(evaluate_real("10 - 4"), 6.0);
  EXPECT_DOUBLE_EQ(evaluate_real("3 * 4"), 12.0);
  EXPECT_DOUBLE_EQ(evaluate_real("15 / 3"), 5.0);
}

TEST_F(TensorEvaluatorTest, ScalarFunctions) {
  EXPECT_NEAR(evaluate_real("sin(0)"), 0.0, 1e-10);
  EXPECT_NEAR(evaluate_real("cos(0)"), 1.0, 1e-10);
  EXPECT_NEAR(evaluate_real("sqrt(4)"), 2.0, 1e-10);
  EXPECT_NEAR(evaluate_real("exp(0)"), 1.0, 1e-10);
}

// Array creation functions

TEST_F(TensorEvaluatorTest, Linspace) {
  XTensor arr = evaluate_array("linspace(0, 10, 11)");

  EXPECT_EQ(arr.size(), 11);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 0.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(10), 10.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(5), 5.0);
}

TEST_F(TensorEvaluatorTest, Arange) {
  XTensor arr = evaluate_array("arange(0, 5, 1)");

  EXPECT_EQ(arr.size(), 5);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 0.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(4), 4.0);
}

TEST_F(TensorEvaluatorTest, Zeros) {
  XTensor arr = evaluate_array("zeros(5)");

  EXPECT_EQ(arr.size(), 5);
  for (size_t i = 0; i < 5; ++i) {
    EXPECT_DOUBLE_EQ(arr.get_real_at(i), 0.0);
  }
}

TEST_F(TensorEvaluatorTest, Ones) {
  XTensor arr = evaluate_array("ones(5)");

  EXPECT_EQ(arr.size(), 5);
  for (size_t i = 0; i < 5; ++i) {
    EXPECT_DOUBLE_EQ(arr.get_real_at(i), 1.0);
  }
}

// Array with scalar operations (broadcasting)

TEST_F(TensorEvaluatorTest, ArrayPlusScalar) {
  XTensor arr = evaluate_array("linspace(0, 4, 5) + 10");

  EXPECT_EQ(arr.size(), 5);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 10.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(4), 14.0);
}

TEST_F(TensorEvaluatorTest, ScalarPlusArray) {
  XTensor arr = evaluate_array("10 + linspace(0, 4, 5)");

  EXPECT_EQ(arr.size(), 5);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 10.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(4), 14.0);
}

TEST_F(TensorEvaluatorTest, ArrayTimesScalar) {
  XTensor arr = evaluate_array("linspace(0, 4, 5) * 2");

  EXPECT_EQ(arr.size(), 5);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 0.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(2), 4.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(4), 8.0);
}

TEST_F(TensorEvaluatorTest, ArrayDivideScalar) {
  XTensor arr = evaluate_array("linspace(0, 10, 5) / 2");

  EXPECT_EQ(arr.size(), 5);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 0.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(4), 5.0);
}

// Reduction functions

TEST_F(TensorEvaluatorTest, Sum) {
  double result = evaluate_real("sum(linspace(1, 5, 5))");
  EXPECT_DOUBLE_EQ(result, 15.0);  // 1+2+3+4+5
}

TEST_F(TensorEvaluatorTest, Mean) {
  double result = evaluate_real("mean(linspace(1, 5, 5))");
  EXPECT_DOUBLE_EQ(result, 3.0);
}

TEST_F(TensorEvaluatorTest, Min) {
  double result = evaluate_real("min(linspace(5, 1, 5))");
  EXPECT_DOUBLE_EQ(result, 1.0);
}

TEST_F(TensorEvaluatorTest, Max) {
  double result = evaluate_real("max(linspace(1, 5, 5))");
  EXPECT_DOUBLE_EQ(result, 5.0);
}

TEST_F(TensorEvaluatorTest, Len) {
  double result = evaluate_real("len(linspace(0, 10, 101))");
  EXPECT_DOUBLE_EQ(result, 101.0);
}

// Element-wise math functions on arrays

TEST_F(TensorEvaluatorTest, SinArray) {
  XTensor arr = evaluate_array("sin(linspace(0, 3.14159265359, 5))");

  EXPECT_EQ(arr.size(), 5);
  EXPECT_NEAR(arr.get_real_at(0), 0.0, 1e-10);
  EXPECT_NEAR(arr.get_real_at(4), 0.0, 1e-5);  // sin(pi) ~ 0
}

TEST_F(TensorEvaluatorTest, ExpArray) {
  XTensor arr = evaluate_array("exp(linspace(0, 2, 3))");

  EXPECT_EQ(arr.size(), 3);
  EXPECT_NEAR(arr.get_real_at(0), 1.0, 1e-10);
  EXPECT_NEAR(arr.get_real_at(1), std::exp(1.0), 1e-10);
  EXPECT_NEAR(arr.get_real_at(2), std::exp(2.0), 1e-10);
}

TEST_F(TensorEvaluatorTest, SqrtArray) {
  XTensor arr = evaluate_array("sqrt(linspace(0, 4, 5))");

  EXPECT_EQ(arr.size(), 5);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 0.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(4), 2.0);
}

TEST_F(TensorEvaluatorTest, AbsArray) {
  XTensor arr = evaluate_array("abs(linspace(-2, 2, 5))");

  EXPECT_EQ(arr.size(), 5);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 2.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(2), 0.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(4), 2.0);
}

// Array from symbol table

TEST_F(TensorEvaluatorTest, ArrayFromSymbolTable) {
  std::vector<double> data = {1.0, 2.0, 3.0, 4.0, 5.0};
  symbols_->define_array("mydata", data);

  XTensor arr = evaluate_array("mydata");
  EXPECT_EQ(arr.size(), 5);
  EXPECT_DOUBLE_EQ(arr.get_real_at(2), 3.0);
}

TEST_F(TensorEvaluatorTest, ArrayOperationsFromSymbolTable) {
  std::vector<double> data = {1.0, 2.0, 3.0, 4.0, 5.0};
  symbols_->define_array("mydata", data);

  double result = evaluate_real("sum(mydata * 2)");
  EXPECT_DOUBLE_EQ(result, 30.0);  // (1+2+3+4+5)*2
}

TEST_F(TensorEvaluatorTest, ComplexArrayFromSymbolTable) {
  std::vector<std::complex<double>> data = {{1.0, 2.0}, {3.0, 4.0}, {5.0, 6.0}};
  symbols_->define_array("cdata", data);

  XTensor arr = evaluate_array("cdata");
  EXPECT_TRUE(arr.is_complex());
  EXPECT_EQ(arr.size(), 3);

  auto val = arr.get_complex_at(1);
  EXPECT_DOUBLE_EQ(val.real(), 3.0);
  EXPECT_DOUBLE_EQ(val.imag(), 4.0);
}

// S-parameter style calculations (post-processing use case)

TEST_F(TensorEvaluatorTest, SParameterMagnitudeDb) {
  std::vector<std::complex<double>> s11 = {
      {0.1, 0.2}, {0.15, 0.25}, {0.12, 0.18}, {0.08, 0.15}, {0.05, 0.1}};
  symbols_->define_array("s11", s11);

  // Calculate |S11| in dB: 20 * log10(abs(s11))
  XTensor s11_db = evaluate_array("20 * log10(abs(s11))");

  EXPECT_EQ(s11_db.size(), 5);
  EXPECT_TRUE(s11_db.is_real());

  double expected = 20.0 * std::log10(std::abs(std::complex<double>(0.1, 0.2)));
  EXPECT_NEAR(s11_db.get_real_at(0), expected, 1e-10);
}

TEST_F(TensorEvaluatorTest, SParameterAverageLoss) {
  std::vector<std::complex<double>> s11 = {
      {0.1, 0.2}, {0.15, 0.25}, {0.12, 0.18}, {0.08, 0.15}, {0.05, 0.1}};
  symbols_->define_array("s11", s11);

  double avg_mag = evaluate_real("mean(abs(s11))");
  EXPECT_GT(avg_mag, 0.0);
  EXPECT_LT(avg_mag, 1.0);
}

// Chained operations

TEST_F(TensorEvaluatorTest, ChainedOperations) {
  XTensor arr = evaluate_array("sqrt(abs(linspace(-4, 4, 9)))");

  EXPECT_EQ(arr.size(), 9);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 2.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(4), 0.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(8), 2.0);
}

// Error cases

TEST_F(TensorEvaluatorTest, LinspaceWrongArgCount) {
  EXPECT_THROW(evaluate("linspace(0, 10)"), EvaluationError);
}

TEST_F(TensorEvaluatorTest, UndefinedArray) {
  EXPECT_THROW(evaluate("undefined_array"), EvaluationError);
}

// XTensor type query tests (replaces old EvalValueOps tests)

TEST_F(TensorEvaluatorTest, ScalarTensorQueries) {
  XTensor scalar_val = XTensor::scalar(42.0);

  EXPECT_TRUE(scalar_val.is_scalar());
  EXPECT_TRUE(scalar_val.is_real());
  EXPECT_FALSE(scalar_val.is_complex());
  EXPECT_DOUBLE_EQ(scalar_val.get_real_at(0), 42.0);
}

TEST_F(TensorEvaluatorTest, ComplexScalarTensorQueries) {
  XTensor complex_val = XTensor::scalar(std::complex<double>(3.0, 4.0));

  EXPECT_TRUE(complex_val.is_scalar());
  EXPECT_FALSE(complex_val.is_real());
  EXPECT_TRUE(complex_val.is_complex());

  auto c = complex_val.get_complex_at(0);
  EXPECT_DOUBLE_EQ(c.real(), 3.0);
  EXPECT_DOUBLE_EQ(c.imag(), 4.0);
}

TEST_F(TensorEvaluatorTest, ArrayTensorQueries) {
  XTensor array_val = evaluate("linspace(0, 10, 11)");

  EXPECT_FALSE(array_val.is_scalar());
  EXPECT_TRUE(array_val.is_real());
  EXPECT_EQ(array_val.size(), 11);
}

}  // namespace
}  // namespace spice_expr
