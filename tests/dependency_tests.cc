#include <gtest/gtest.h>

#include "spice_expr/spice_expr.h"

using namespace spice_expr;

class DependencyTest : public ::testing::Test {
 protected:
  ExprArena arena;
};

TEST_F(DependencyTest, AddNode) {
  DependencyGraph graph;
  graph.add_node("a");
  graph.add_node("b");

  auto nodes = graph.all_nodes();
  EXPECT_EQ(nodes.size(), 2);
}

TEST_F(DependencyTest, AddDependency) {
  DependencyGraph graph;
  graph.add_dependency("a", "b");

  auto deps = graph.get_dependencies("a");
  EXPECT_EQ(deps.size(), 1);
  EXPECT_TRUE(deps.count("b") > 0);

  auto dependents = graph.get_dependents("b");
  EXPECT_EQ(dependents.size(), 1);
  EXPECT_TRUE(dependents.count("a") > 0);
}

TEST_F(DependencyTest, RemoveDependency) {
  DependencyGraph graph;
  graph.add_dependency("a", "b");
  graph.remove_dependency("a", "b");

  auto deps = graph.get_dependencies("a");
  EXPECT_EQ(deps.size(), 0);
}

TEST_F(DependencyTest, NoCycle) {
  DependencyGraph graph;
  graph.add_dependency("c", "b");
  graph.add_dependency("b", "a");

  EXPECT_FALSE(graph.has_cycle());
}

TEST_F(DependencyTest, DetectCycle) {
  DependencyGraph graph;
  graph.add_dependency("a", "b");
  graph.add_dependency("b", "c");
  graph.add_dependency("c", "a");

  EXPECT_TRUE(graph.has_cycle());
}

TEST_F(DependencyTest, FindCycle) {
  DependencyGraph graph;
  graph.add_dependency("a", "b");
  graph.add_dependency("b", "c");
  graph.add_dependency("c", "a");

  auto cycle = graph.find_cycle();
  ASSERT_TRUE(cycle.has_value());
  EXPECT_GE(cycle->size(), 3);
}

TEST_F(DependencyTest, TopologicalSort) {
  DependencyGraph graph;
  graph.add_dependency("c", "b");
  graph.add_dependency("b", "a");
  graph.add_dependency("c", "a");

  auto sorted = graph.topological_sort();
  ASSERT_TRUE(sorted.has_value());
  EXPECT_EQ(sorted->size(), 3);

  auto findPos = [&](const std::string& name) {
    return std::find(sorted->begin(), sorted->end(), name) - sorted->begin();
  };

  EXPECT_LT(findPos("a"), findPos("b"));
  EXPECT_LT(findPos("b"), findPos("c"));
}

TEST_F(DependencyTest, TopologicalSortWithCycle) {
  DependencyGraph graph;
  graph.add_dependency("a", "b");
  graph.add_dependency("b", "a");

  auto sorted = graph.topological_sort();
  EXPECT_FALSE(sorted.has_value());
}

TEST_F(DependencyTest, DependencyExtractor) {
  auto* expr =
      arena.make<BinaryOp>(BinaryOpType::Add, arena.make<Identifier>("a"),
                           arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<Identifier>("b"),
                                                arena.make<CircuitNodeRef>("1")));

  DependencyExtractor extractor;
  auto deps = extractor.extract(*expr);

  EXPECT_EQ(deps.parameters.size(), 2);
  EXPECT_TRUE(deps.parameters.count("a") > 0);
  EXPECT_TRUE(deps.parameters.count("b") > 0);
  EXPECT_EQ(deps.nodeVoltages.size(), 1);
  EXPECT_TRUE(deps.nodeVoltages.count("1") > 0);
}

TEST_F(DependencyTest, DependencyExtractorFunctions) {
  auto* expr = arena.make<FunctionCall>("sin", std::vector<ExprNode*>{arena.make<Identifier>("x")});

  DependencyExtractor extractor;
  auto deps = extractor.extract(*expr);

  EXPECT_EQ(deps.functions.size(), 1);
  EXPECT_TRUE(deps.functions.count("sin") > 0);
  EXPECT_EQ(deps.parameters.size(), 1);
  EXPECT_TRUE(deps.parameters.count("x") > 0);
}

TEST_F(DependencyTest, EvaluationOrderManager) {
  SymbolTable symbols;
  symbols.define("a", arena.make<NumberLiteral>(1.0));
  symbols.define("b", arena.make<Identifier>("a"));
  symbols.define("c", arena.make<BinaryOp>(BinaryOpType::Add, arena.make<Identifier>("a"),
                                           arena.make<Identifier>("b")));

  EvaluationOrderManager manager;
  manager.build_from_symbol_table(symbols);

  EXPECT_TRUE(manager.is_valid());
  EXPECT_FALSE(manager.has_cycle());

  const auto& order = manager.evaluation_order();
  EXPECT_EQ(order.size(), 3);

  auto findPos = [&](const std::string& name) {
    return std::find(order.begin(), order.end(), name) - order.begin();
  };

  EXPECT_LT(findPos("a"), findPos("b"));
  EXPECT_LT(findPos("a"), findPos("c"));
  EXPECT_LT(findPos("b"), findPos("c"));
}

TEST_F(DependencyTest, EvaluationOrderWithCycle) {
  SymbolTable symbols;
  symbols.define("a", arena.make<Identifier>("b"));
  symbols.define("b", arena.make<Identifier>("a"));

  EvaluationOrderManager manager;
  manager.build_from_symbol_table(symbols);

  EXPECT_FALSE(manager.is_valid());
  EXPECT_TRUE(manager.has_cycle());

  auto cycle = manager.get_cycle();
  EXPECT_TRUE(cycle.has_value());
}

TEST_F(DependencyTest, InvalidateOrder) {
  SymbolTable symbols;
  symbols.define("a", arena.make<NumberLiteral>(1.0));

  EvaluationOrderManager manager;
  manager.build_from_symbol_table(symbols);
  EXPECT_TRUE(manager.is_valid());

  manager.invalidate();
  EXPECT_FALSE(manager.is_valid());
}

TEST_F(DependencyTest, GetNode) {
  DependencyGraph graph;
  graph.add_node("test");

  const DependencyNode* node = graph.get_node("test");
  ASSERT_NE(node, nullptr);
  EXPECT_EQ(node->name, "test");

  EXPECT_EQ(graph.get_node("nonexistent"), nullptr);
}

TEST_F(DependencyTest, ClearGraph) {
  DependencyGraph graph;
  graph.add_node("a");
  graph.add_node("b");
  graph.add_dependency("a", "b");

  graph.clear();
  EXPECT_EQ(graph.all_nodes().size(), 0);
}

TEST_F(DependencyTest, DiamondDependency) {
  DependencyGraph graph;
  graph.add_dependency("d", "b");
  graph.add_dependency("d", "c");
  graph.add_dependency("b", "a");
  graph.add_dependency("c", "a");

  EXPECT_FALSE(graph.has_cycle());

  auto sorted = graph.topological_sort();
  ASSERT_TRUE(sorted.has_value());

  auto findPos = [&](const std::string& name) {
    return std::find(sorted->begin(), sorted->end(), name) - sorted->begin();
  };

  EXPECT_LT(findPos("a"), findPos("b"));
  EXPECT_LT(findPos("a"), findPos("c"));
  EXPECT_LT(findPos("b"), findPos("d"));
  EXPECT_LT(findPos("c"), findPos("d"));
}

TEST_F(DependencyTest, TransitiveDependentsSingle) {
  DependencyGraph graph;
  // a <- b <- c (c depends on b, b depends on a)
  graph.add_dependency("b", "a");
  graph.add_dependency("c", "b");

  auto deps = graph.get_transitive_dependents("a");
  EXPECT_EQ(deps.size(), 2);
  EXPECT_TRUE(deps.count("b") > 0);
  EXPECT_TRUE(deps.count("c") > 0);
}

TEST_F(DependencyTest, TransitiveDependentsNone) {
  DependencyGraph graph;
  graph.add_node("a");
  graph.add_node("b");

  auto deps = graph.get_transitive_dependents("a");
  EXPECT_TRUE(deps.empty());
}

TEST_F(DependencyTest, TransitiveDependentsDiamond) {
  DependencyGraph graph;
  // a is at the root, b and c depend on a, d depends on both b and c
  graph.add_dependency("b", "a");
  graph.add_dependency("c", "a");
  graph.add_dependency("d", "b");
  graph.add_dependency("d", "c");

  auto deps = graph.get_transitive_dependents("a");
  EXPECT_EQ(deps.size(), 3);
  EXPECT_TRUE(deps.count("b") > 0);
  EXPECT_TRUE(deps.count("c") > 0);
  EXPECT_TRUE(deps.count("d") > 0);
}

TEST_F(DependencyTest, TransitiveDependentsNonexistent) {
  DependencyGraph graph;
  graph.add_node("a");

  auto deps = graph.get_transitive_dependents("nonexistent");
  EXPECT_TRUE(deps.empty());
}

TEST_F(DependencyTest, TransitiveDependentsMultiple) {
  DependencyGraph graph;
  // a <- b, c <- d (two separate chains)
  graph.add_dependency("b", "a");
  graph.add_dependency("d", "c");

  std::set<std::string> sources{"a", "c"};
  auto deps = graph.get_transitive_dependents(sources);
  EXPECT_EQ(deps.size(), 2);
  EXPECT_TRUE(deps.count("b") > 0);
  EXPECT_TRUE(deps.count("d") > 0);
}

TEST_F(DependencyTest, EvaluationOrderAffectedParameters) {
  SymbolTable symbols;
  symbols.define("a", arena.make<NumberLiteral>(1.0));
  symbols.define("b", arena.make<Identifier>("a"));
  symbols.define("c", arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<Identifier>("b"),
                                           arena.make<NumberLiteral>(2.0)));

  EvaluationOrderManager manager;
  manager.build_from_symbol_table(symbols);

  auto affected = manager.get_affected_parameters("a");
  EXPECT_EQ(affected.size(), 2);
  EXPECT_TRUE(affected.count("b") > 0);
  EXPECT_TRUE(affected.count("c") > 0);
}

TEST_F(DependencyTest, EvaluationOrderAffectedParametersMultiple) {
  SymbolTable symbols;
  symbols.define("a", arena.make<NumberLiteral>(1.0));
  symbols.define("b", arena.make<NumberLiteral>(2.0));
  symbols.define("c", arena.make<Identifier>("a"));
  symbols.define("d", arena.make<Identifier>("b"));

  EvaluationOrderManager manager;
  manager.build_from_symbol_table(symbols);

  std::set<std::string> sources{"a", "b"};
  auto affected = manager.get_affected_parameters(sources);
  EXPECT_EQ(affected.size(), 2);
  EXPECT_TRUE(affected.count("c") > 0);
  EXPECT_TRUE(affected.count("d") > 0);
}
