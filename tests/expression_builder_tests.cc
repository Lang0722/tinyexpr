#include <gtest/gtest.h>

#include "spice_expr/spice_expr.h"

using namespace spice_expr;

class ExpressionBuilderTest : public ::testing::Test {
 protected:
  ExprArena arena;
  ExpressionBuilder b{arena};
};

// --- Operand pushers ---

TEST_F(ExpressionBuilderTest, NumPushesNumberLiteral) {
  auto* node = b.num(3.14).build();
  ASSERT_EQ(node->type(), NodeType::NumberLiteral);
  auto* lit = static_cast<NumberLiteral*>(node);
  EXPECT_DOUBLE_EQ(lit->real(), 3.14);
  EXPECT_FALSE(lit->is_complex());
}

TEST_F(ExpressionBuilderTest, ComplexPushesComplexLiteral) {
  auto* node = b.complex(1.0, 2.0).build();
  ASSERT_EQ(node->type(), NodeType::NumberLiteral);
  auto* lit = static_cast<NumberLiteral*>(node);
  EXPECT_TRUE(lit->is_complex());
  EXPECT_DOUBLE_EQ(lit->real(), 1.0);
  EXPECT_DOUBLE_EQ(lit->imag(), 2.0);
}

TEST_F(ExpressionBuilderTest, StrPushesStringLiteral) {
  auto* node = b.str("hello").build();
  ASSERT_EQ(node->type(), NodeType::StringLiteral);
  EXPECT_EQ(static_cast<StringLiteral*>(node)->value(), "hello");
}

TEST_F(ExpressionBuilderTest, IdPushesIdentifier) {
  auto* node = b.id("x").build();
  ASSERT_EQ(node->type(), NodeType::Identifier);
  EXPECT_EQ(static_cast<Identifier*>(node)->name(), "x");
}

TEST_F(ExpressionBuilderTest, VoltageSingleNode) {
  auto* node = b.voltage("out").build();
  ASSERT_EQ(node->type(), NodeType::CircuitNodeRef);
  auto* ref = static_cast<CircuitNodeRef*>(node);
  EXPECT_EQ(ref->node1(), "out");
  EXPECT_FALSE(ref->is_differential());
}

TEST_F(ExpressionBuilderTest, VoltageDifferential) {
  auto* node = b.voltage("inp", "inn").build();
  ASSERT_EQ(node->type(), NodeType::CircuitNodeRef);
  auto* ref = static_cast<CircuitNodeRef*>(node);
  EXPECT_EQ(ref->node1(), "inp");
  EXPECT_EQ(ref->node2(), "inn");
  EXPECT_TRUE(ref->is_differential());
}

TEST_F(ExpressionBuilderTest, CurrentPushesCircuitCurrentRef) {
  auto* node = b.current("R1").build();
  ASSERT_EQ(node->type(), NodeType::CircuitCurrentRef);
  EXPECT_EQ(static_cast<CircuitCurrentRef*>(node)->device(), "R1");
}

TEST_F(ExpressionBuilderTest, PushExistingNode) {
  auto* existing = arena.make<NumberLiteral>(42.0);
  auto* node = b.push(existing).build();
  EXPECT_EQ(node, existing);
}

// --- Binary operators ---

TEST_F(ExpressionBuilderTest, AddCorrectOrdering) {
  auto* node = b.num(1.0).num(2.0).add().build();
  ASSERT_EQ(node->type(), NodeType::BinaryOp);
  auto* op = static_cast<BinaryOp*>(node);
  EXPECT_EQ(op->op_type(), BinaryOpType::Add);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(op->left())->real(), 1.0);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(op->right())->real(), 2.0);
}

TEST_F(ExpressionBuilderTest, SubCorrectOrdering) {
  auto* node = b.num(5.0).num(3.0).sub().build();
  auto* op = static_cast<BinaryOp*>(node);
  EXPECT_EQ(op->op_type(), BinaryOpType::Subtract);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(op->left())->real(), 5.0);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(op->right())->real(), 3.0);
}

TEST_F(ExpressionBuilderTest, MulCorrectOrdering) {
  auto* node = b.num(2.0).num(4.0).mul().build();
  auto* op = static_cast<BinaryOp*>(node);
  EXPECT_EQ(op->op_type(), BinaryOpType::Multiply);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(op->left())->real(), 2.0);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(op->right())->real(), 4.0);
}

TEST_F(ExpressionBuilderTest, DivCorrectOrdering) {
  auto* node = b.num(10.0).num(2.0).div().build();
  auto* op = static_cast<BinaryOp*>(node);
  EXPECT_EQ(op->op_type(), BinaryOpType::Divide);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(op->left())->real(), 10.0);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(op->right())->real(), 2.0);
}

TEST_F(ExpressionBuilderTest, PowCorrectOrdering) {
  auto* node = b.num(2.0).num(3.0).pow().build();
  auto* op = static_cast<BinaryOp*>(node);
  EXPECT_EQ(op->op_type(), BinaryOpType::Power);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(op->left())->real(), 2.0);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(op->right())->real(), 3.0);
}

TEST_F(ExpressionBuilderTest, ModCorrectOrdering) {
  auto* node = b.num(7.0).num(3.0).mod().build();
  auto* op = static_cast<BinaryOp*>(node);
  EXPECT_EQ(op->op_type(), BinaryOpType::Modulo);
}

TEST_F(ExpressionBuilderTest, ComparisonOperators) {
  auto test_cmp = [this](auto method, BinaryOpType expected) {
    b.clear();
    b.num(1.0).num(2.0);
    (b.*method)();
    auto* op = static_cast<BinaryOp*>(b.build());
    EXPECT_EQ(op->op_type(), expected);
  };
  test_cmp(&ExpressionBuilder::eq, BinaryOpType::Equal);
  test_cmp(&ExpressionBuilder::ne, BinaryOpType::NotEqual);
  test_cmp(&ExpressionBuilder::lt, BinaryOpType::Less);
  test_cmp(&ExpressionBuilder::le, BinaryOpType::LessEqual);
  test_cmp(&ExpressionBuilder::gt, BinaryOpType::Greater);
  test_cmp(&ExpressionBuilder::ge, BinaryOpType::GreaterEqual);
}

TEST_F(ExpressionBuilderTest, LogicalOperators) {
  auto* node = b.num(1.0).num(0.0).land().build();
  EXPECT_EQ(static_cast<BinaryOp*>(node)->op_type(), BinaryOpType::LogicalAnd);

  auto* node2 = b.num(1.0).num(0.0).lor().build();
  EXPECT_EQ(static_cast<BinaryOp*>(node2)->op_type(), BinaryOpType::LogicalOr);
}

// --- Unary operators ---

TEST_F(ExpressionBuilderTest, NegCreatesNegate) {
  auto* node = b.num(5.0).neg().build();
  ASSERT_EQ(node->type(), NodeType::UnaryOp);
  auto* op = static_cast<UnaryOp*>(node);
  EXPECT_EQ(op->op_type(), UnaryOpType::Negate);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(op->operand())->real(), 5.0);
}

TEST_F(ExpressionBuilderTest, LnotCreatesLogicalNot) {
  auto* node = b.num(1.0).lnot().build();
  ASSERT_EQ(node->type(), NodeType::UnaryOp);
  auto* op = static_cast<UnaryOp*>(node);
  EXPECT_EQ(op->op_type(), UnaryOpType::LogicalNot);
}

// --- Function calls ---

TEST_F(ExpressionBuilderTest, CallZeroArgs) {
  auto* node = b.call("rand", 0).build();
  ASSERT_EQ(node->type(), NodeType::FunctionCall);
  auto* fn = static_cast<FunctionCall*>(node);
  EXPECT_EQ(fn->name(), "rand");
  EXPECT_EQ(fn->argument_count(), 0);
}

TEST_F(ExpressionBuilderTest, Call1SingleArg) {
  auto* node = b.num(3.14).call1("sin").build();
  ASSERT_EQ(node->type(), NodeType::FunctionCall);
  auto* fn = static_cast<FunctionCall*>(node);
  EXPECT_EQ(fn->name(), "sin");
  ASSERT_EQ(fn->argument_count(), 1);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(fn->arguments()[0])->real(), 3.14);
}

TEST_F(ExpressionBuilderTest, Call2TwoArgs) {
  auto* node = b.num(2.0).num(3.0).call2("pow").build();
  ASSERT_EQ(node->type(), NodeType::FunctionCall);
  auto* fn = static_cast<FunctionCall*>(node);
  EXPECT_EQ(fn->name(), "pow");
  ASSERT_EQ(fn->argument_count(), 2);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(fn->arguments()[0])->real(), 2.0);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(fn->arguments()[1])->real(), 3.0);
}

TEST_F(ExpressionBuilderTest, CallThreeArgsOrdering) {
  auto* node = b.num(1.0).num(2.0).num(3.0).call("clamp", 3).build();
  auto* fn = static_cast<FunctionCall*>(node);
  ASSERT_EQ(fn->argument_count(), 3);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(fn->arguments()[0])->real(), 1.0);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(fn->arguments()[1])->real(), 2.0);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(fn->arguments()[2])->real(), 3.0);
}

// --- Ternary ---

TEST_F(ExpressionBuilderTest, TernaryCorrectOrdering) {
  // cond ? true : false  =>  push cond, push true, push false, ternary
  auto* node = b.id("flag").num(1.0).num(0.0).ternary().build();
  ASSERT_EQ(node->type(), NodeType::TernaryConditional);
  auto* t = static_cast<TernaryConditional*>(node);
  EXPECT_EQ(t->condition()->type(), NodeType::Identifier);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(t->true_expr())->real(), 1.0);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(t->false_expr())->real(), 0.0);
}

// --- Array literal ---

TEST_F(ExpressionBuilderTest, ArrayCorrectOrdering) {
  auto* node = b.num(10.0).num(20.0).num(30.0).array(3).build();
  ASSERT_EQ(node->type(), NodeType::ArrayLiteral);
  auto* arr = static_cast<ArrayLiteral*>(node);
  ASSERT_EQ(arr->element_count(), 3);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(arr->elements()[0])->real(), 10.0);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(arr->elements()[1])->real(), 20.0);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(arr->elements()[2])->real(), 30.0);
}

// --- Array index ---

TEST_F(ExpressionBuilderTest, IndexCorrectOrdering) {
  // array[index]: push array, push index, index()
  auto* node = b.id("arr").num(2.0).index().build();
  ASSERT_EQ(node->type(), NodeType::ArrayIndex);
  auto* ai = static_cast<ArrayIndex*>(node);
  EXPECT_EQ(ai->array()->type(), NodeType::Identifier);
  EXPECT_EQ(ai->index()->type(), NodeType::NumberLiteral);
}

// --- Stack underflow ---

TEST_F(ExpressionBuilderTest, BuildOnEmptyStackThrows) {
  EXPECT_THROW(b.build(), std::runtime_error);
}

TEST_F(ExpressionBuilderTest, ResultOnEmptyStackThrows) {
  EXPECT_THROW(b.result(), std::runtime_error);
}

TEST_F(ExpressionBuilderTest, AddUnderflowThrows) {
  b.num(1.0);
  EXPECT_THROW(b.add(), std::runtime_error);
}

TEST_F(ExpressionBuilderTest, NegUnderflowThrows) {
  EXPECT_THROW(b.neg(), std::runtime_error);
}

TEST_F(ExpressionBuilderTest, TernaryUnderflowThrows) {
  b.num(1.0).num(2.0);
  EXPECT_THROW(b.ternary(), std::runtime_error);
}

TEST_F(ExpressionBuilderTest, CallUnderflowThrows) {
  b.num(1.0);
  EXPECT_THROW(b.call("f", 2), std::runtime_error);
}

// --- Clear and depth ---

TEST_F(ExpressionBuilderTest, ClearResetsStack) {
  b.num(1.0).num(2.0).num(3.0);
  EXPECT_EQ(b.depth(), 3);
  b.clear();
  EXPECT_EQ(b.depth(), 0);
  EXPECT_THROW(b.build(), std::runtime_error);
}

TEST_F(ExpressionBuilderTest, DepthTracksCorrectly) {
  EXPECT_EQ(b.depth(), 0);
  b.num(1.0);
  EXPECT_EQ(b.depth(), 1);
  b.num(2.0);
  EXPECT_EQ(b.depth(), 2);
  b.add();
  EXPECT_EQ(b.depth(), 1);
  b.build();
  EXPECT_EQ(b.depth(), 0);
}

// --- Chained expressions ---

TEST_F(ExpressionBuilderTest, ChainedExpressionMatchesManualAST) {
  // Build: 2*x + 1  using RPN: 2 x * 1 +
  auto* rpn = b.num(2.0).id("x").mul().num(1.0).add().build();

  // Build equivalent manually
  auto* manual = arena.make<BinaryOp>(
      BinaryOpType::Add,
      arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<NumberLiteral>(2.0),
                           arena.make<Identifier>("x")),
      arena.make<NumberLiteral>(1.0));

  EXPECT_TRUE(rpn->equals(*manual));
}

TEST_F(ExpressionBuilderTest, NestedFunctionExpression) {
  // sin(x + 1): x 1 + sin
  auto* rpn = b.id("x").num(1.0).add().call1("sin").build();

  auto* manual = arena.make<FunctionCall>(
      "sin", std::vector<ExprNode*>{arena.make<BinaryOp>(
                 BinaryOpType::Add, arena.make<Identifier>("x"), arena.make<NumberLiteral>(1.0))});

  EXPECT_TRUE(rpn->equals(*manual));
}

// --- Builder reuse ---

TEST_F(ExpressionBuilderTest, ReuseAfterBuild) {
  auto* first = b.num(1.0).build();
  EXPECT_EQ(b.depth(), 0);

  auto* second = b.num(2.0).build();
  EXPECT_EQ(b.depth(), 0);

  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(first)->real(), 1.0);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(second)->real(), 2.0);
}

TEST_F(ExpressionBuilderTest, ReuseAfterClear) {
  b.num(1.0).num(2.0).num(3.0);
  b.clear();
  auto* node = b.num(42.0).build();
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(node)->real(), 42.0);
}

// --- Result peeks without popping ---

TEST_F(ExpressionBuilderTest, ResultDoesNotPop) {
  b.num(5.0);
  auto* first = b.result();
  auto* second = b.result();
  EXPECT_EQ(first, second);
  EXPECT_EQ(b.depth(), 1);
}
