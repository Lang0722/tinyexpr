#include <gtest/gtest.h>

#include "spice_expr/spice_expr.h"

using namespace spice_expr;

class AstTest : public ::testing::Test {
 protected:
  ExprArena arena;
};

TEST_F(AstTest, NumberLiteralReal) {
  auto* node = arena.make<NumberLiteral>(3.14);
  EXPECT_EQ(node->type(), NodeType::NumberLiteral);
  EXPECT_DOUBLE_EQ(node->real(), 3.14);
  EXPECT_DOUBLE_EQ(node->imag(), 0.0);
  EXPECT_FALSE(node->is_complex());
}

TEST_F(AstTest, NumberLiteralComplex) {
  auto* node = arena.make<NumberLiteral>(1.0, 2.0);
  EXPECT_TRUE(node->is_complex());
  EXPECT_DOUBLE_EQ(node->real(), 1.0);
  EXPECT_DOUBLE_EQ(node->imag(), 2.0);
  auto c = node->complex_value();
  EXPECT_DOUBLE_EQ(c.real(), 1.0);
  EXPECT_DOUBLE_EQ(c.imag(), 2.0);
}

TEST_F(AstTest, StringLiteral) {
  auto* node = arena.make<StringLiteral>("hello");
  EXPECT_EQ(node->type(), NodeType::StringLiteral);
  EXPECT_EQ(node->value(), "hello");
}

TEST_F(AstTest, ArrayLiteral) {
  std::vector<ExprNode*> elements = {arena.make<NumberLiteral>(1.0), arena.make<NumberLiteral>(2.0),
                                     arena.make<NumberLiteral>(3.0)};
  auto* node = arena.make<ArrayLiteral>(elements);
  EXPECT_EQ(node->type(), NodeType::ArrayLiteral);
  EXPECT_EQ(node->element_count(), 3);
}

TEST_F(AstTest, Identifier) {
  auto* node = arena.make<Identifier>("vdd");
  EXPECT_EQ(node->type(), NodeType::Identifier);
  EXPECT_EQ(node->name(), "vdd");
}

TEST_F(AstTest, CircuitNodeRefSingle) {
  auto* node = arena.make<CircuitNodeRef>("out");
  EXPECT_EQ(node->type(), NodeType::CircuitNodeRef);
  EXPECT_EQ(node->node1(), "out");
  EXPECT_FALSE(node->is_differential());
}

TEST_F(AstTest, CircuitNodeRefDifferential) {
  auto* node = arena.make<CircuitNodeRef>("inp", "inn");
  EXPECT_TRUE(node->is_differential());
  EXPECT_EQ(node->node1(), "inp");
  EXPECT_EQ(node->node2(), "inn");
}

TEST_F(AstTest, CircuitCurrentRef) {
  auto* node = arena.make<CircuitCurrentRef>("R1");
  EXPECT_EQ(node->type(), NodeType::CircuitCurrentRef);
  EXPECT_EQ(node->device(), "R1");
}

TEST_F(AstTest, BinaryOp) {
  auto* left = arena.make<NumberLiteral>(2.0);
  auto* right = arena.make<NumberLiteral>(3.0);
  auto* node = arena.make<BinaryOp>(BinaryOpType::Add, left, right);
  EXPECT_EQ(node->type(), NodeType::BinaryOp);
  EXPECT_EQ(node->op_type(), BinaryOpType::Add);
  EXPECT_EQ(node->left(), left);
  EXPECT_EQ(node->right(), right);
}

TEST_F(AstTest, UnaryOp) {
  auto* operand = arena.make<NumberLiteral>(5.0);
  auto* node = arena.make<UnaryOp>(UnaryOpType::Negate, operand);
  EXPECT_EQ(node->type(), NodeType::UnaryOp);
  EXPECT_EQ(node->op_type(), UnaryOpType::Negate);
  EXPECT_EQ(node->operand(), operand);
}

TEST_F(AstTest, FunctionCall) {
  std::vector<ExprNode*> args = {arena.make<NumberLiteral>(0.5)};
  auto* node = arena.make<FunctionCall>("sin", args);
  EXPECT_EQ(node->type(), NodeType::FunctionCall);
  EXPECT_EQ(node->name(), "sin");
  EXPECT_EQ(node->argument_count(), 1);
}

TEST_F(AstTest, ArrayIndex) {
  auto* arr = arena.make<Identifier>("data");
  auto* idx = arena.make<NumberLiteral>(0.0);
  auto* node = arena.make<ArrayIndex>(arr, idx);
  EXPECT_EQ(node->type(), NodeType::ArrayIndex);
  EXPECT_EQ(node->array(), arr);
  EXPECT_EQ(node->index(), idx);
}

TEST_F(AstTest, TernaryConditional) {
  auto* cond = arena.make<Identifier>("flag");
  auto* true_expr = arena.make<NumberLiteral>(1.0);
  auto* false_expr = arena.make<NumberLiteral>(0.0);
  auto* node = arena.make<TernaryConditional>(cond, true_expr, false_expr);
  EXPECT_EQ(node->type(), NodeType::TernaryConditional);
  EXPECT_EQ(node->condition(), cond);
  EXPECT_EQ(node->true_expr(), true_expr);
  EXPECT_EQ(node->false_expr(), false_expr);
}

TEST_F(AstTest, CloneNumberLiteral) {
  auto* orig = arena.make<NumberLiteral>(42.0);
  ExprArena arena2;
  auto* clone = orig->clone(arena2);
  EXPECT_NE(orig, clone);
  EXPECT_TRUE(orig->equals(*clone));
}

TEST_F(AstTest, CloneBinaryOp) {
  auto* left = arena.make<NumberLiteral>(1.0);
  auto* right = arena.make<NumberLiteral>(2.0);
  auto* orig = arena.make<BinaryOp>(BinaryOpType::Add, left, right);

  ExprArena arena2;
  auto* clone = orig->clone(arena2);
  EXPECT_NE(orig, clone);
  EXPECT_TRUE(orig->equals(*clone));
}

TEST_F(AstTest, HashConsistency) {
  auto* a = arena.make<NumberLiteral>(3.14);
  auto* b = arena.make<NumberLiteral>(3.14);
  EXPECT_EQ(a->hash(), b->hash());

  auto* c = arena.make<NumberLiteral>(2.71);
  EXPECT_NE(a->hash(), c->hash());
}

TEST_F(AstTest, EqualityDifferentTypes) {
  auto* num = arena.make<NumberLiteral>(1.0);
  auto* str = arena.make<StringLiteral>("1.0");
  EXPECT_FALSE(num->equals(*str));
}

TEST_F(AstTest, SourceLocation) {
  auto* node = arena.make<NumberLiteral>(1.0);
  node->set_location(SourceLocation(10, 5, "test.sp"));
  EXPECT_EQ(node->location().line, 10);
  EXPECT_EQ(node->location().column, 5);
  EXPECT_EQ(node->location().filename, "test.sp");
}

TEST_F(AstTest, ArenaOwnership) {
  {
    ExprArena tempArena;
    for (int i = 0; i < 100; i++) {
      tempArena.make<NumberLiteral>(static_cast<double>(i));
    }
    EXPECT_EQ(tempArena.size(), 100);
  }
}

TEST_F(AstTest, BinaryOpTypeToString) {
  EXPECT_STREQ(binaryOpTypeToString(BinaryOpType::Add), "+");
  EXPECT_STREQ(binaryOpTypeToString(BinaryOpType::Multiply), "*");
  EXPECT_STREQ(binaryOpTypeToString(BinaryOpType::Power), "**");
}

TEST_F(AstTest, UnaryOpTypeToString) {
  EXPECT_STREQ(unaryOpTypeToString(UnaryOpType::Negate), "-");
  EXPECT_STREQ(unaryOpTypeToString(UnaryOpType::LogicalNot), "!");
}
