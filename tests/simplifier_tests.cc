#include <gtest/gtest.h>

#include "spice_expr/spice_expr.h"

using namespace spice_expr;

class SimplifierTest : public ::testing::Test {
 protected:
  ExprArena arena;

  ExprNode* simplify(const ExprNode& node) {
    Simplifier simplifier(arena);
    return simplifier.simplify_to_fixpoint(node);
  }
};

// =============================================================================
// Common Subexpression Tests
// =============================================================================

TEST_F(SimplifierTest, IdenticalAdditionSimple) {
  // x + x -> 2 * x
  auto* x = arena.make<Identifier>("x");
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Add, x, x);

  auto* simplified = simplify(*expr);

  ASSERT_EQ(simplified->type(), NodeType::BinaryOp);
  auto* binop = static_cast<BinaryOp*>(simplified);
  EXPECT_EQ(binop->op_type(), BinaryOpType::Multiply);

  // Should be 2 * x
  ASSERT_EQ(binop->left()->type(), NodeType::NumberLiteral);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(binop->left())->real(), 2.0);
  ASSERT_EQ(binop->right()->type(), NodeType::Identifier);
  EXPECT_EQ(static_cast<Identifier*>(binop->right())->name(), "x");
}

TEST_F(SimplifierTest, IdenticalMultiplicationSimple) {
  // x * x -> x ^ 2
  auto* x = arena.make<Identifier>("x");
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Multiply, x, x);

  auto* simplified = simplify(*expr);

  ASSERT_EQ(simplified->type(), NodeType::BinaryOp);
  auto* binop = static_cast<BinaryOp*>(simplified);
  EXPECT_EQ(binop->op_type(), BinaryOpType::Power);

  // Should be x ^ 2
  ASSERT_EQ(binop->left()->type(), NodeType::Identifier);
  EXPECT_EQ(static_cast<Identifier*>(binop->left())->name(), "x");
  ASSERT_EQ(binop->right()->type(), NodeType::NumberLiteral);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(binop->right())->real(), 2.0);
}

TEST_F(SimplifierTest, IdenticalSubexpressionAddition) {
  // (a + b) + (a + b) -> 2 * (a + b)
  auto* a = arena.make<Identifier>("a");
  auto* b = arena.make<Identifier>("b");
  auto* ab1 = arena.make<BinaryOp>(BinaryOpType::Add, a, b);
  auto* ab2 = arena.make<BinaryOp>(BinaryOpType::Add,
                                   arena.make<Identifier>("a"),
                                   arena.make<Identifier>("b"));
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Add, ab1, ab2);

  auto* simplified = simplify(*expr);

  ASSERT_EQ(simplified->type(), NodeType::BinaryOp);
  auto* binop = static_cast<BinaryOp*>(simplified);
  EXPECT_EQ(binop->op_type(), BinaryOpType::Multiply);
}

TEST_F(SimplifierTest, IdenticalSubexpressionMultiplication) {
  // (a + b) * (a + b) -> (a + b) ^ 2
  auto* ab1 = arena.make<BinaryOp>(BinaryOpType::Add,
                                   arena.make<Identifier>("a"),
                                   arena.make<Identifier>("b"));
  auto* ab2 = arena.make<BinaryOp>(BinaryOpType::Add,
                                   arena.make<Identifier>("a"),
                                   arena.make<Identifier>("b"));
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Multiply, ab1, ab2);

  auto* simplified = simplify(*expr);

  ASSERT_EQ(simplified->type(), NodeType::BinaryOp);
  auto* binop = static_cast<BinaryOp*>(simplified);
  EXPECT_EQ(binop->op_type(), BinaryOpType::Power);
}

TEST_F(SimplifierTest, IdenticalSubtractionToZero) {
  // (a + b) - (a + b) -> 0
  auto* ab1 = arena.make<BinaryOp>(BinaryOpType::Add,
                                   arena.make<Identifier>("a"),
                                   arena.make<Identifier>("b"));
  auto* ab2 = arena.make<BinaryOp>(BinaryOpType::Add,
                                   arena.make<Identifier>("a"),
                                   arena.make<Identifier>("b"));
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Subtract, ab1, ab2);

  auto* simplified = simplify(*expr);

  ASSERT_EQ(simplified->type(), NodeType::NumberLiteral);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(simplified)->real(), 0.0);
}

TEST_F(SimplifierTest, IdenticalDivisionToOne) {
  // (a + b) / (a + b) -> 1
  auto* ab1 = arena.make<BinaryOp>(BinaryOpType::Add,
                                   arena.make<Identifier>("a"),
                                   arena.make<Identifier>("b"));
  auto* ab2 = arena.make<BinaryOp>(BinaryOpType::Add,
                                   arena.make<Identifier>("a"),
                                   arena.make<Identifier>("b"));
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Divide, ab1, ab2);

  auto* simplified = simplify(*expr);

  ASSERT_EQ(simplified->type(), NodeType::NumberLiteral);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(simplified)->real(), 1.0);
}

TEST_F(SimplifierTest, NonIdenticalNoChange) {
  // x + y should not be simplified to multiplication
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Add,
                                    arena.make<Identifier>("x"),
                                    arena.make<Identifier>("y"));

  auto* simplified = simplify(*expr);

  ASSERT_EQ(simplified->type(), NodeType::BinaryOp);
  EXPECT_EQ(static_cast<BinaryOp*>(simplified)->op_type(), BinaryOpType::Add);
}

// =============================================================================
// Commutativity Exploitation Tests
// =============================================================================

TEST_F(SimplifierTest, CommutativeConstantFoldingAddRightNested) {
  // (x + 2) + 3 -> x + 5
  auto* x_plus_2 = arena.make<BinaryOp>(BinaryOpType::Add,
                                        arena.make<Identifier>("x"),
                                        arena.make<NumberLiteral>(2.0));
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Add, x_plus_2,
                                    arena.make<NumberLiteral>(3.0));

  auto* simplified = simplify(*expr);

  ASSERT_EQ(simplified->type(), NodeType::BinaryOp);
  auto* binop = static_cast<BinaryOp*>(simplified);
  EXPECT_EQ(binop->op_type(), BinaryOpType::Add);

  // Find x and 5 in the result
  bool found_x = false, found_5 = false;
  auto check = [&](ExprNode* n) {
    if (n->type() == NodeType::Identifier) {
      found_x = static_cast<Identifier*>(n)->name() == "x";
    } else if (n->type() == NodeType::NumberLiteral) {
      found_5 = static_cast<NumberLiteral*>(n)->real() == 5.0;
    }
  };
  check(binop->left());
  check(binop->right());
  EXPECT_TRUE(found_x);
  EXPECT_TRUE(found_5);
}

TEST_F(SimplifierTest, CommutativeConstantFoldingAddLeftNested) {
  // (2 + x) + 3 -> x + 5
  auto* two_plus_x = arena.make<BinaryOp>(BinaryOpType::Add,
                                          arena.make<NumberLiteral>(2.0),
                                          arena.make<Identifier>("x"));
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Add, two_plus_x,
                                    arena.make<NumberLiteral>(3.0));

  auto* simplified = simplify(*expr);

  ASSERT_EQ(simplified->type(), NodeType::BinaryOp);
  auto* binop = static_cast<BinaryOp*>(simplified);
  EXPECT_EQ(binop->op_type(), BinaryOpType::Add);

  // Verify constants were folded
  bool found_x = false, found_5 = false;
  auto check = [&](ExprNode* n) {
    if (n->type() == NodeType::Identifier) {
      found_x = static_cast<Identifier*>(n)->name() == "x";
    } else if (n->type() == NodeType::NumberLiteral) {
      found_5 = static_cast<NumberLiteral*>(n)->real() == 5.0;
    }
  };
  check(binop->left());
  check(binop->right());
  EXPECT_TRUE(found_x);
  EXPECT_TRUE(found_5);
}

TEST_F(SimplifierTest, CommutativeConstantFoldingAddSwapped) {
  // 3 + (x + 2) -> x + 5
  auto* x_plus_2 = arena.make<BinaryOp>(BinaryOpType::Add,
                                        arena.make<Identifier>("x"),
                                        arena.make<NumberLiteral>(2.0));
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Add,
                                    arena.make<NumberLiteral>(3.0), x_plus_2);

  auto* simplified = simplify(*expr);

  ASSERT_EQ(simplified->type(), NodeType::BinaryOp);
  auto* binop = static_cast<BinaryOp*>(simplified);
  EXPECT_EQ(binop->op_type(), BinaryOpType::Add);

  // Verify constants were folded
  bool found_x = false, found_5 = false;
  auto check = [&](ExprNode* n) {
    if (n->type() == NodeType::Identifier) {
      found_x = static_cast<Identifier*>(n)->name() == "x";
    } else if (n->type() == NodeType::NumberLiteral) {
      found_5 = static_cast<NumberLiteral*>(n)->real() == 5.0;
    }
  };
  check(binop->left());
  check(binop->right());
  EXPECT_TRUE(found_x);
  EXPECT_TRUE(found_5);
}

TEST_F(SimplifierTest, CommutativeConstantFoldingMultiply) {
  // (x * 2) * 3 -> x * 6
  auto* x_times_2 = arena.make<BinaryOp>(BinaryOpType::Multiply,
                                         arena.make<Identifier>("x"),
                                         arena.make<NumberLiteral>(2.0));
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Multiply, x_times_2,
                                    arena.make<NumberLiteral>(3.0));

  auto* simplified = simplify(*expr);

  ASSERT_EQ(simplified->type(), NodeType::BinaryOp);
  auto* binop = static_cast<BinaryOp*>(simplified);
  EXPECT_EQ(binop->op_type(), BinaryOpType::Multiply);

  // Verify constants were folded to 6
  bool found_x = false, found_6 = false;
  auto check = [&](ExprNode* n) {
    if (n->type() == NodeType::Identifier) {
      found_x = static_cast<Identifier*>(n)->name() == "x";
    } else if (n->type() == NodeType::NumberLiteral) {
      found_6 = static_cast<NumberLiteral*>(n)->real() == 6.0;
    }
  };
  check(binop->left());
  check(binop->right());
  EXPECT_TRUE(found_x);
  EXPECT_TRUE(found_6);
}

TEST_F(SimplifierTest, CommutativeConstantFoldingDeep) {
  // ((1 + x) + 2) + 3 -> x + 6
  auto* one_plus_x = arena.make<BinaryOp>(BinaryOpType::Add,
                                          arena.make<NumberLiteral>(1.0),
                                          arena.make<Identifier>("x"));
  auto* plus_2 = arena.make<BinaryOp>(BinaryOpType::Add, one_plus_x,
                                      arena.make<NumberLiteral>(2.0));
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Add, plus_2,
                                    arena.make<NumberLiteral>(3.0));

  auto* simplified = simplify(*expr);

  ASSERT_EQ(simplified->type(), NodeType::BinaryOp);
  auto* binop = static_cast<BinaryOp*>(simplified);
  EXPECT_EQ(binop->op_type(), BinaryOpType::Add);

  // Verify all constants were folded to 6
  bool found_x = false, found_6 = false;
  auto check = [&](ExprNode* n) {
    if (n->type() == NodeType::Identifier) {
      found_x = static_cast<Identifier*>(n)->name() == "x";
    } else if (n->type() == NodeType::NumberLiteral) {
      found_6 = static_cast<NumberLiteral*>(n)->real() == 6.0;
    }
  };
  check(binop->left());
  check(binop->right());
  EXPECT_TRUE(found_x);
  EXPECT_TRUE(found_6);
}

TEST_F(SimplifierTest, CommutativeConstantFoldingMixed) {
  // (x + 2) + (y + 3) -> should fold 2 and 3 to 5
  auto* x_plus_2 = arena.make<BinaryOp>(BinaryOpType::Add,
                                        arena.make<Identifier>("x"),
                                        arena.make<NumberLiteral>(2.0));
  auto* y_plus_3 = arena.make<BinaryOp>(BinaryOpType::Add,
                                        arena.make<Identifier>("y"),
                                        arena.make<NumberLiteral>(3.0));
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Add, x_plus_2, y_plus_3);

  auto* simplified = simplify(*expr);

  // The constants 2 and 3 should be folded to 5
  // Result structure: ((x + y) + 5) or similar
  ASSERT_EQ(simplified->type(), NodeType::BinaryOp);
  auto* binop = static_cast<BinaryOp*>(simplified);
  EXPECT_EQ(binop->op_type(), BinaryOpType::Add);

  // Check that 5 appears somewhere in the result and 2, 3 do not
  std::function<bool(const ExprNode*, double)> contains_value;
  contains_value = [&](const ExprNode* n, double val) -> bool {
    if (n->type() == NodeType::NumberLiteral) {
      return static_cast<const NumberLiteral*>(n)->real() == val;
    }
    if (n->type() == NodeType::BinaryOp) {
      auto* b = static_cast<const BinaryOp*>(n);
      return contains_value(b->left(), val) || contains_value(b->right(), val);
    }
    return false;
  };

  EXPECT_TRUE(contains_value(simplified, 5.0));
  EXPECT_FALSE(contains_value(simplified, 2.0));
  EXPECT_FALSE(contains_value(simplified, 3.0));
}

TEST_F(SimplifierTest, CommutativeNoChangeIfNoConstants) {
  // (x + y) + z -> no constant folding (no constants to fold)
  auto* x_plus_y = arena.make<BinaryOp>(BinaryOpType::Add,
                                        arena.make<Identifier>("x"),
                                        arena.make<Identifier>("y"));
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Add, x_plus_y,
                                    arena.make<Identifier>("z"));

  auto* simplified = simplify(*expr);

  // Structure should be preserved (no simplification possible)
  ASSERT_EQ(simplified->type(), NodeType::BinaryOp);
  EXPECT_EQ(static_cast<BinaryOp*>(simplified)->op_type(), BinaryOpType::Add);
}

// =============================================================================
// Edge Cases
// =============================================================================

TEST_F(SimplifierTest, ZeroTimesExpressionStillZero) {
  // 0 * (x + x) -> 0
  auto* x_plus_x = arena.make<BinaryOp>(BinaryOpType::Add,
                                        arena.make<Identifier>("x"),
                                        arena.make<Identifier>("x"));
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Multiply,
                                    arena.make<NumberLiteral>(0.0), x_plus_x);

  auto* simplified = simplify(*expr);

  ASSERT_EQ(simplified->type(), NodeType::NumberLiteral);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(simplified)->real(), 0.0);
}

TEST_F(SimplifierTest, MixedOperatorsNoConfusion) {
  // (x + 2) * 3 should NOT combine 2 and 3
  auto* x_plus_2 = arena.make<BinaryOp>(BinaryOpType::Add,
                                        arena.make<Identifier>("x"),
                                        arena.make<NumberLiteral>(2.0));
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Multiply, x_plus_2,
                                    arena.make<NumberLiteral>(3.0));

  auto* simplified = simplify(*expr);

  // Should remain (x + 2) * 3
  ASSERT_EQ(simplified->type(), NodeType::BinaryOp);
  EXPECT_EQ(static_cast<BinaryOp*>(simplified)->op_type(), BinaryOpType::Multiply);

  // The 2 should still be present in the addition
  auto* left = static_cast<BinaryOp*>(simplified)->left();
  ASSERT_EQ(left->type(), NodeType::BinaryOp);
  auto* add_op = static_cast<BinaryOp*>(left);
  EXPECT_EQ(add_op->op_type(), BinaryOpType::Add);
}

TEST_F(SimplifierTest, ConstantFoldingToIdentityAdd) {
  // (x + 3) + (-3) -> x + 0 -> x
  auto* x_plus_3 = arena.make<BinaryOp>(BinaryOpType::Add,
                                        arena.make<Identifier>("x"),
                                        arena.make<NumberLiteral>(3.0));
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Add, x_plus_3,
                                    arena.make<NumberLiteral>(-3.0));

  auto* simplified = simplify(*expr);

  // Should simplify to just x (constants fold to 0, then 0+x -> x)
  ASSERT_EQ(simplified->type(), NodeType::Identifier);
  EXPECT_EQ(static_cast<Identifier*>(simplified)->name(), "x");
}

TEST_F(SimplifierTest, ConstantFoldingToIdentityMultiply) {
  // (x * 2) * 0.5 -> x * 1 -> x
  auto* x_times_2 = arena.make<BinaryOp>(BinaryOpType::Multiply,
                                         arena.make<Identifier>("x"),
                                         arena.make<NumberLiteral>(2.0));
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Multiply, x_times_2,
                                    arena.make<NumberLiteral>(0.5));

  auto* simplified = simplify(*expr);

  // Should simplify to just x (constants fold to 1, then x*1 -> x)
  ASSERT_EQ(simplified->type(), NodeType::Identifier);
  EXPECT_EQ(static_cast<Identifier*>(simplified)->name(), "x");
}

TEST_F(SimplifierTest, AllConstantsFullyFold) {
  // (1 + 2) + 3 -> 6
  auto* one_plus_two = arena.make<BinaryOp>(BinaryOpType::Add,
                                            arena.make<NumberLiteral>(1.0),
                                            arena.make<NumberLiteral>(2.0));
  auto* expr = arena.make<BinaryOp>(BinaryOpType::Add, one_plus_two,
                                    arena.make<NumberLiteral>(3.0));

  auto* simplified = simplify(*expr);

  ASSERT_EQ(simplified->type(), NodeType::NumberLiteral);
  EXPECT_DOUBLE_EQ(static_cast<NumberLiteral*>(simplified)->real(), 6.0);
}
