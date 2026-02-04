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
#include "spice_expr/visitor/array_evaluator.h"

namespace spice_expr {
namespace {

class ArrayEvaluatorTest : public ::testing::Test {
 protected:
  void SetUp() override {
    arena_ = std::make_unique<ExprArena>();
    symbols_ = std::make_unique<SymbolTable>();
    functions_ = std::make_unique<FunctionRegistry>();
    circuit_ = std::make_unique<NullCircuitInterface>();
  }

  EvalValue evaluate(const std::string& expr_str) {
    ExprNode* expr = parse_expression(expr_str, *arena_);
    ArrayEvaluator evaluator(*symbols_, *functions_, *circuit_, arena_.get());
    return evaluator.evaluate(*expr);
  }

  double evaluate_real(const std::string& expr_str) {
    EvalValue val = evaluate(expr_str);
    return EvalValueOps::to_real(val);
  }

  XTensor evaluate_array(const std::string& expr_str) {
    EvalValue val = evaluate(expr_str);
    if (!EvalValueOps::is_array(val)) {
      throw std::runtime_error("Expected array result");
    }
    return std::get<XTensor>(val);
  }

  std::unique_ptr<ExprArena> arena_;
  std::unique_ptr<SymbolTable> symbols_;
  std::unique_ptr<FunctionRegistry> functions_;
  std::unique_ptr<NullCircuitInterface> circuit_;
};

// Basic scalar evaluation (should still work)

TEST_F(ArrayEvaluatorTest, ScalarLiteral) {
  double result = evaluate_real("42.0");
  EXPECT_DOUBLE_EQ(result, 42.0);
}

TEST_F(ArrayEvaluatorTest, ScalarArithmetic) {
  EXPECT_DOUBLE_EQ(evaluate_real("2 + 3"), 5.0);
  EXPECT_DOUBLE_EQ(evaluate_real("10 - 4"), 6.0);
  EXPECT_DOUBLE_EQ(evaluate_real("3 * 4"), 12.0);
  EXPECT_DOUBLE_EQ(evaluate_real("15 / 3"), 5.0);
}

TEST_F(ArrayEvaluatorTest, ScalarFunctions) {
  EXPECT_NEAR(evaluate_real("sin(0)"), 0.0, 1e-10);
  EXPECT_NEAR(evaluate_real("cos(0)"), 1.0, 1e-10);
  EXPECT_NEAR(evaluate_real("sqrt(4)"), 2.0, 1e-10);
  EXPECT_NEAR(evaluate_real("exp(0)"), 1.0, 1e-10);
}

// Array creation functions

TEST_F(ArrayEvaluatorTest, Linspace) {
  XTensor arr = evaluate_array("linspace(0, 10, 11)");

  EXPECT_EQ(arr.size(), 11);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 0.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(10), 10.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(5), 5.0);
}

TEST_F(ArrayEvaluatorTest, Arange) {
  XTensor arr = evaluate_array("arange(0, 5, 1)");

  EXPECT_EQ(arr.size(), 5);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 0.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(4), 4.0);
}

TEST_F(ArrayEvaluatorTest, Zeros) {
  XTensor arr = evaluate_array("zeros(5)");

  EXPECT_EQ(arr.size(), 5);
  for (size_t i = 0; i < 5; ++i) {
    EXPECT_DOUBLE_EQ(arr.get_real_at(i), 0.0);
  }
}

TEST_F(ArrayEvaluatorTest, Ones) {
  XTensor arr = evaluate_array("ones(5)");

  EXPECT_EQ(arr.size(), 5);
  for (size_t i = 0; i < 5; ++i) {
    EXPECT_DOUBLE_EQ(arr.get_real_at(i), 1.0);
  }
}

// Array with scalar operations (broadcasting)

TEST_F(ArrayEvaluatorTest, ArrayPlusScalar) {
  XTensor arr = evaluate_array("linspace(0, 4, 5) + 10");

  EXPECT_EQ(arr.size(), 5);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 10.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(4), 14.0);
}

TEST_F(ArrayEvaluatorTest, ScalarPlusArray) {
  XTensor arr = evaluate_array("10 + linspace(0, 4, 5)");

  EXPECT_EQ(arr.size(), 5);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 10.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(4), 14.0);
}

TEST_F(ArrayEvaluatorTest, ArrayTimesScalar) {
  XTensor arr = evaluate_array("linspace(0, 4, 5) * 2");

  EXPECT_EQ(arr.size(), 5);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 0.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(2), 4.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(4), 8.0);
}

TEST_F(ArrayEvaluatorTest, ArrayDivideScalar) {
  XTensor arr = evaluate_array("linspace(0, 10, 5) / 2");

  EXPECT_EQ(arr.size(), 5);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 0.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(4), 5.0);
}

// Reduction functions

TEST_F(ArrayEvaluatorTest, Sum) {
  double result = evaluate_real("sum(linspace(1, 5, 5))");
  EXPECT_DOUBLE_EQ(result, 15.0);  // 1+2+3+4+5
}

TEST_F(ArrayEvaluatorTest, Mean) {
  double result = evaluate_real("mean(linspace(1, 5, 5))");
  EXPECT_DOUBLE_EQ(result, 3.0);
}

TEST_F(ArrayEvaluatorTest, Min) {
  double result = evaluate_real("min(linspace(5, 1, 5))");
  EXPECT_DOUBLE_EQ(result, 1.0);
}

TEST_F(ArrayEvaluatorTest, Max) {
  double result = evaluate_real("max(linspace(1, 5, 5))");
  EXPECT_DOUBLE_EQ(result, 5.0);
}

TEST_F(ArrayEvaluatorTest, Len) {
  double result = evaluate_real("len(linspace(0, 10, 101))");
  EXPECT_DOUBLE_EQ(result, 101.0);
}

// Element-wise math functions on arrays

TEST_F(ArrayEvaluatorTest, SinArray) {
  XTensor arr = evaluate_array("sin(linspace(0, 3.14159265359, 5))");

  EXPECT_EQ(arr.size(), 5);
  EXPECT_NEAR(arr.get_real_at(0), 0.0, 1e-10);
  EXPECT_NEAR(arr.get_real_at(4), 0.0, 1e-5);  // sin(pi) ≈ 0
}

TEST_F(ArrayEvaluatorTest, ExpArray) {
  XTensor arr = evaluate_array("exp(linspace(0, 2, 3))");

  EXPECT_EQ(arr.size(), 3);
  EXPECT_NEAR(arr.get_real_at(0), 1.0, 1e-10);        // exp(0) = 1
  EXPECT_NEAR(arr.get_real_at(1), std::exp(1.0), 1e-10);  // exp(1)
  EXPECT_NEAR(arr.get_real_at(2), std::exp(2.0), 1e-10);  // exp(2)
}

TEST_F(ArrayEvaluatorTest, SqrtArray) {
  XTensor arr = evaluate_array("sqrt(linspace(0, 4, 5))");

  EXPECT_EQ(arr.size(), 5);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 0.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(4), 2.0);
}

TEST_F(ArrayEvaluatorTest, AbsArray) {
  XTensor arr = evaluate_array("abs(linspace(-2, 2, 5))");

  EXPECT_EQ(arr.size(), 5);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 2.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(2), 0.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(4), 2.0);
}

// Array from symbol table

TEST_F(ArrayEvaluatorTest, ArrayFromSymbolTable) {
  std::vector<double> data = {1.0, 2.0, 3.0, 4.0, 5.0};
  symbols_->define_array("mydata", data);

  XTensor arr = evaluate_array("mydata");
  EXPECT_EQ(arr.size(), 5);
  EXPECT_DOUBLE_EQ(arr.get_real_at(2), 3.0);
}

TEST_F(ArrayEvaluatorTest, ArrayOperationsFromSymbolTable) {
  std::vector<double> data = {1.0, 2.0, 3.0, 4.0, 5.0};
  symbols_->define_array("mydata", data);

  double result = evaluate_real("sum(mydata * 2)");
  EXPECT_DOUBLE_EQ(result, 30.0);  // (1+2+3+4+5)*2
}

TEST_F(ArrayEvaluatorTest, ComplexArrayFromSymbolTable) {
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

TEST_F(ArrayEvaluatorTest, SParameterMagnitudeDb) {
  // Simulate S11 data
  std::vector<std::complex<double>> s11 = {
      {0.1, 0.2}, {0.15, 0.25}, {0.12, 0.18}, {0.08, 0.15}, {0.05, 0.1}};
  symbols_->define_array("s11", s11);

  // Calculate |S11| in dB: 20 * log10(abs(s11))
  XTensor s11_db = evaluate_array("20 * log10(abs(s11))");

  EXPECT_EQ(s11_db.size(), 5);
  EXPECT_TRUE(s11_db.is_real());

  // Verify first value: |0.1 + 0.2j| = sqrt(0.01 + 0.04) = sqrt(0.05) ≈ 0.2236
  // 20 * log10(0.2236) ≈ -13.01 dB
  double expected = 20.0 * std::log10(std::abs(std::complex<double>(0.1, 0.2)));
  EXPECT_NEAR(s11_db.get_real_at(0), expected, 1e-10);
}

TEST_F(ArrayEvaluatorTest, SParameterAverageLoss) {
  std::vector<std::complex<double>> s11 = {
      {0.1, 0.2}, {0.15, 0.25}, {0.12, 0.18}, {0.08, 0.15}, {0.05, 0.1}};
  symbols_->define_array("s11", s11);

  // Calculate average magnitude
  double avg_mag = evaluate_real("mean(abs(s11))");
  EXPECT_GT(avg_mag, 0.0);
  EXPECT_LT(avg_mag, 1.0);
}

// Chained operations

TEST_F(ArrayEvaluatorTest, ChainedOperations) {
  XTensor arr = evaluate_array("sqrt(abs(linspace(-4, 4, 9)))");

  EXPECT_EQ(arr.size(), 9);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 2.0);  // sqrt(abs(-4)) = 2
  EXPECT_DOUBLE_EQ(arr.get_real_at(4), 0.0);  // sqrt(abs(0)) = 0
  EXPECT_DOUBLE_EQ(arr.get_real_at(8), 2.0);  // sqrt(abs(4)) = 2
}

// Error cases

TEST_F(ArrayEvaluatorTest, LinspaceWrongArgCount) {
  EXPECT_THROW(evaluate("linspace(0, 10)"), EvaluationError);
}

TEST_F(ArrayEvaluatorTest, UndefinedArray) {
  EXPECT_THROW(evaluate("undefined_array"), EvaluationError);
}

// EvalValueOps tests

TEST_F(ArrayEvaluatorTest, EvalValueOpsScalar) {
  EvalValue scalar_val = 42.0;

  EXPECT_TRUE(EvalValueOps::is_scalar(scalar_val));
  EXPECT_FALSE(EvalValueOps::is_array(scalar_val));
  EXPECT_TRUE(EvalValueOps::is_real(scalar_val));
  EXPECT_DOUBLE_EQ(EvalValueOps::to_real(scalar_val), 42.0);
}

TEST_F(ArrayEvaluatorTest, EvalValueOpsComplex) {
  EvalValue complex_val = std::complex<double>(3.0, 4.0);

  EXPECT_TRUE(EvalValueOps::is_scalar(complex_val));
  EXPECT_FALSE(EvalValueOps::is_array(complex_val));
  EXPECT_TRUE(EvalValueOps::is_complex(complex_val));

  auto c = EvalValueOps::to_complex(complex_val);
  EXPECT_DOUBLE_EQ(c.real(), 3.0);
  EXPECT_DOUBLE_EQ(c.imag(), 4.0);
}

TEST_F(ArrayEvaluatorTest, EvalValueOpsArray) {
  EvalValue array_val = evaluate("linspace(0, 10, 11)");

  EXPECT_FALSE(EvalValueOps::is_scalar(array_val));
  EXPECT_TRUE(EvalValueOps::is_array(array_val));
}

}  // namespace
}  // namespace spice_expr
