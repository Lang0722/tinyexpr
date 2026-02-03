#include <gtest/gtest.h>

#include "spice_expr/spice_expr.h"

using namespace spice_expr;

class ArrayTest : public ::testing::Test {
 protected:
  ExprArena arena;
};

TEST_F(ArrayTest, RealArrayConstruction) {
  ArrayValue arr(std::vector<double>{1.0, 2.0, 3.0});
  EXPECT_TRUE(arr.is_real());
  EXPECT_FALSE(arr.is_complex());
  EXPECT_EQ(arr.size(), 3);
}

TEST_F(ArrayTest, ComplexArrayConstruction) {
  ArrayValue arr(std::vector<std::complex<double>>{{1.0, 2.0}, {3.0, 4.0}});
  EXPECT_FALSE(arr.is_real());
  EXPECT_TRUE(arr.is_complex());
  EXPECT_EQ(arr.size(), 2);
}

TEST_F(ArrayTest, EmptyArray) {
  ArrayValue arr(std::vector<double>{});
  EXPECT_TRUE(arr.is_empty());
  EXPECT_EQ(arr.size(), 0);
}

TEST_F(ArrayTest, GetRealAt) {
  ArrayValue arr(std::vector<double>{1.0, 2.0, 3.0});
  EXPECT_DOUBLE_EQ(arr.get_real_at(0), 1.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(1), 2.0);
  EXPECT_DOUBLE_EQ(arr.get_real_at(2), 3.0);
}

TEST_F(ArrayTest, GetComplexAt) {
  ArrayValue arr(std::vector<std::complex<double>>{{1.0, 2.0}, {3.0, 4.0}});
  auto c0 = arr.get_complex_at(0);
  EXPECT_DOUBLE_EQ(c0.real(), 1.0);
  EXPECT_DOUBLE_EQ(c0.imag(), 2.0);
}

TEST_F(ArrayTest, RealData) {
  ArrayValue arr(std::vector<double>{1.0, 2.0, 3.0});
  const auto& data = arr.real_data();
  EXPECT_EQ(data.size(), 3);
  EXPECT_DOUBLE_EQ(data[0], 1.0);
}

TEST_F(ArrayTest, ComplexData) {
  ArrayValue arr(std::vector<std::complex<double>>{{1.0, 2.0}});
  const auto& data = arr.complex_data();
  EXPECT_EQ(data.size(), 1);
}

TEST_F(ArrayTest, ToComplex) {
  ArrayValue real(std::vector<double>{1.0, 2.0, 3.0});
  ArrayValue complex = real.to_complex();
  EXPECT_TRUE(complex.is_complex());
  EXPECT_EQ(complex.size(), 3);
  EXPECT_DOUBLE_EQ(complex.get_complex_at(0).real(), 1.0);
  EXPECT_DOUBLE_EQ(complex.get_complex_at(0).imag(), 0.0);
}

TEST_F(ArrayTest, Sum) {
  ArrayValue arr(std::vector<double>{1.0, 2.0, 3.0, 4.0});
  EXPECT_DOUBLE_EQ(ArrayValue::sum(arr), 10.0);
}

TEST_F(ArrayTest, Avg) {
  ArrayValue arr(std::vector<double>{1.0, 2.0, 3.0, 4.0});
  EXPECT_DOUBLE_EQ(ArrayValue::avg(arr), 2.5);
}

TEST_F(ArrayTest, Length) {
  ArrayValue arr(std::vector<double>{1.0, 2.0, 3.0});
  EXPECT_DOUBLE_EQ(ArrayValue::length(arr), 3.0);
}

TEST_F(ArrayTest, OutOfBoundsThrows) {
  ArrayValue arr(std::vector<double>{1.0, 2.0});
  EXPECT_THROW(arr.get_real_at(5), ArrayError);
}

TEST_F(ArrayTest, AvgEmptyThrows) {
  ArrayValue arr(std::vector<double>{});
  EXPECT_THROW(ArrayValue::avg(arr), ArrayError);
}

TEST_F(ArrayTest, WrongTypeThrows) {
  ArrayValue real(std::vector<double>{1.0, 2.0});
  EXPECT_THROW(real.complex_data(), ArrayError);

  ArrayValue complex(std::vector<std::complex<double>>{{1.0, 2.0}});
  EXPECT_THROW(complex.real_data(), ArrayError);
}

TEST_F(ArrayTest, ArrayLiteralNode) {
  std::vector<ExprNode*> elements = {arena.make<NumberLiteral>(1.0), arena.make<NumberLiteral>(2.0),
                                     arena.make<NumberLiteral>(3.0)};
  auto* node = arena.make<ArrayLiteral>(elements);

  EXPECT_EQ(node->element_count(), 3);
  EXPECT_EQ(node->elements()[0]->type(), NodeType::NumberLiteral);
}

TEST_F(ArrayTest, ArrayIndexNode) {
  auto* arr = arena.make<ArrayLiteral>(std::vector<ExprNode*>{arena.make<NumberLiteral>(10.0),
                                                              arena.make<NumberLiteral>(20.0),
                                                              arena.make<NumberLiteral>(30.0)});
  auto* idx = arena.make<NumberLiteral>(1.0);
  auto* node = arena.make<ArrayIndex>(arr, idx);

  EXPECT_EQ(node->type(), NodeType::ArrayIndex);
  EXPECT_EQ(node->array(), arr);
  EXPECT_EQ(node->index(), idx);
}

TEST_F(ArrayTest, CloneArrayLiteral) {
  auto* orig = arena.make<ArrayLiteral>(
      std::vector<ExprNode*>{arena.make<NumberLiteral>(1.0), arena.make<NumberLiteral>(2.0)});

  ExprArena arena2;
  auto* clone = orig->clone(arena2);
  EXPECT_TRUE(orig->equals(*clone));
  EXPECT_NE(orig, clone);
}

TEST_F(ArrayTest, ArrayLiteralHash) {
  auto* arr1 = arena.make<ArrayLiteral>(
      std::vector<ExprNode*>{arena.make<NumberLiteral>(1.0), arena.make<NumberLiteral>(2.0)});

  auto* arr2 = arena.make<ArrayLiteral>(
      std::vector<ExprNode*>{arena.make<NumberLiteral>(1.0), arena.make<NumberLiteral>(2.0)});

  EXPECT_EQ(arr1->hash(), arr2->hash());
}

TEST_F(ArrayTest, ArrayLiteralEquality) {
  auto* arr1 = arena.make<ArrayLiteral>(
      std::vector<ExprNode*>{arena.make<NumberLiteral>(1.0), arena.make<NumberLiteral>(2.0)});

  auto* arr2 = arena.make<ArrayLiteral>(
      std::vector<ExprNode*>{arena.make<NumberLiteral>(1.0), arena.make<NumberLiteral>(2.0)});

  auto* arr3 = arena.make<ArrayLiteral>(
      std::vector<ExprNode*>{arena.make<NumberLiteral>(1.0), arena.make<NumberLiteral>(3.0)});

  EXPECT_TRUE(arr1->equals(*arr2));
  EXPECT_FALSE(arr1->equals(*arr3));
}
