#include <gtest/gtest.h>

#include <cmath>
#include <complex>
#include <vector>

#include "spice_expr/array/xtensor.h"

namespace spice_expr {
namespace {

class XTensorTest : public ::testing::Test {
 protected:
  void SetUp() override {}
};

// Construction tests

TEST_F(XTensorTest, DefaultConstruction) {
  XTensor arr;
  EXPECT_TRUE(arr.is_real());
  EXPECT_EQ(arr.size(), 0);
}

TEST_F(XTensorTest, ConstructFromRealVector) {
  std::vector<double> data = {1.0, 2.0, 3.0, 4.0, 5.0};
  XTensor arr(data);

  EXPECT_TRUE(arr.is_real());
  EXPECT_FALSE(arr.is_complex());
  EXPECT_EQ(arr.size(), 5);
  EXPECT_EQ(arr.ndim(), 1);

  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 1.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(4), 5.0);
}

TEST_F(XTensorTest, ConstructFromComplexVector) {
  std::vector<std::complex<double>> data = {{1.0, 2.0}, {3.0, 4.0}, {5.0, 6.0}};
  XTensor arr(data);

  EXPECT_FALSE(arr.is_real());
  EXPECT_TRUE(arr.is_complex());
  EXPECT_EQ(arr.size(), 3);

  auto val = arr.get_complex_at(1);
  EXPECT_DOUBLE_EQ(val.real(), 3.0);
  EXPECT_DOUBLE_EQ(val.imag(), 4.0);
}

TEST_F(XTensorTest, ConstructFromInitializerList) {
  XTensor arr{1.0, 2.0, 3.0};

  EXPECT_TRUE(arr.is_real());
  EXPECT_EQ(arr.size(), 3);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 1.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(2), 3.0);
}

// Factory function tests

TEST_F(XTensorTest, Linspace) {
  XTensor arr = linspace(0.0, 10.0, 11);

  EXPECT_TRUE(arr.is_real());
  EXPECT_EQ(arr.size(), 11);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 0.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(10), 10.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(5), 5.0);
}

TEST_F(XTensorTest, Arange) {
  XTensor arr = arange(0.0, 5.0, 1.0);

  EXPECT_TRUE(arr.is_real());
  EXPECT_EQ(arr.size(), 5);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 0.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(4), 4.0);
}

TEST_F(XTensorTest, Zeros) {
  XTensor arr = zeros(5);

  EXPECT_TRUE(arr.is_real());
  EXPECT_EQ(arr.size(), 5);
  for (size_t i = 0; i < 5; ++i) {
    EXPECT_DOUBLE_EQ(arr.get_real_at(i), 0.0);
  }
}

TEST_F(XTensorTest, Ones) {
  XTensor arr = ones(5);

  EXPECT_TRUE(arr.is_real());
  EXPECT_EQ(arr.size(), 5);
  for (size_t i = 0; i < 5; ++i) {
    EXPECT_DOUBLE_EQ(arr.get_real_at(i), 1.0);
  }
}

// Reduction tests

TEST_F(XTensorTest, SumReal) {
  XTensor arr{1.0, 2.0, 3.0, 4.0, 5.0};
  EXPECT_DOUBLE_EQ(arr.sum_real(), 15.0);
}

TEST_F(XTensorTest, MeanReal) {
  XTensor arr{1.0, 2.0, 3.0, 4.0, 5.0};
  EXPECT_DOUBLE_EQ(arr.mean_real(), 3.0);
}

TEST_F(XTensorTest, MinReal) {
  XTensor arr{3.0, 1.0, 4.0, 1.0, 5.0};
  EXPECT_DOUBLE_EQ(arr.min_real(), 1.0);
}

TEST_F(XTensorTest, MaxReal) {
  XTensor arr{3.0, 1.0, 4.0, 1.0, 5.0};
  EXPECT_DOUBLE_EQ(arr.max_real(), 5.0);
}

// Type conversion tests

TEST_F(XTensorTest, ToComplex) {
  XTensor real_arr{1.0, 2.0, 3.0};
  XTensor complex_arr = real_arr.to_complex();

  EXPECT_TRUE(complex_arr.is_complex());
  EXPECT_EQ(complex_arr.size(), 3);

  auto val = complex_arr.get_complex_at(1);
  EXPECT_DOUBLE_EQ(val.real(), 2.0);
  EXPECT_DOUBLE_EQ(val.imag(), 0.0);
}

TEST_F(XTensorTest, GetComplexFromReal) {
  XTensor arr{1.0, 2.0, 3.0};
  auto val = arr.get_complex_at(1);
  EXPECT_DOUBLE_EQ(val.real(), 2.0);
  EXPECT_DOUBLE_EQ(val.imag(), 0.0);
}

// Shape tests

TEST_F(XTensorTest, Shape1D) {
  XTensor arr{1.0, 2.0, 3.0, 4.0};
  auto shape = arr.shape();

  EXPECT_EQ(shape.size(), 1);
  EXPECT_EQ(shape[0], 4);
}

// Edge cases

TEST_F(XTensorTest, EmptyArray) {
  std::vector<double> empty;
  XTensor arr(empty);

  EXPECT_TRUE(arr.is_empty());
  EXPECT_EQ(arr.size(), 0);
}

TEST_F(XTensorTest, SingleElement) {
  XTensor arr{42.0};

  EXPECT_EQ(arr.size(), 1);
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 42.0);
  EXPECT_DOUBLE_EQ(arr.sum_real(), 42.0);
  EXPECT_DOUBLE_EQ(arr.mean_real(), 42.0);
}

}  // namespace
}  // namespace spice_expr
