#include <gtest/gtest.h>

#include "spice_expr/spice_expr.h"

using namespace spice_expr;

class DependencyPrinterTest : public ::testing::Test {
 protected:
  ExprArena arena;
};

TEST_F(DependencyPrinterTest, SingleScopeNoDependencies) {
  SymbolTable symbols;
  symbols.define("vdd", arena.make<NumberLiteral>(1.8));
  symbols.define("vss", arena.make<NumberLiteral>(0.0));

  DependencyPrinter printer;
  std::string output = printer.print(symbols, "root");

  EXPECT_NE(output.find("Scope: root"), std::string::npos);
  EXPECT_NE(output.find("vdd"), std::string::npos);
  EXPECT_NE(output.find("vss"), std::string::npos);
  EXPECT_NE(output.find("(no dependencies)"), std::string::npos);
}

TEST_F(DependencyPrinterTest, SingleScopeWithDependencies) {
  SymbolTable symbols;
  symbols.define("vdd", arena.make<NumberLiteral>(1.8));
  symbols.define("vth", arena.make<BinaryOp>(BinaryOpType::Multiply, arena.make<NumberLiteral>(0.4),
                                             arena.make<Identifier>("vdd")));

  DependencyPrinter printer;
  std::string output = printer.print(symbols, "root");

  EXPECT_NE(output.find("vth"), std::string::npos);
  EXPECT_NE(output.find("depends on: vdd"), std::string::npos);
}

TEST_F(DependencyPrinterTest, HierarchyWithCrossScopeDependency) {
  SymbolTable root(ScopingMode::Local);
  root.define("vdd", arena.make<NumberLiteral>(1.8));

  auto child = root.create_child();
  child->define("ids", arena.make<Identifier>("vdd"));

  ScopeHierarchy hierarchy{&root, "root", {{child.get(), "inverter", {}}}};

  DependencyPrinter printer;
  std::string output = printer.print(hierarchy);

  EXPECT_NE(output.find("Scope: root"), std::string::npos);
  EXPECT_NE(output.find("Scope: root/inverter"), std::string::npos);
  EXPECT_NE(output.find("ids"), std::string::npos);
  EXPECT_NE(output.find("(cross-scope)"), std::string::npos);
  EXPECT_NE(output.find("Cross-scope Summary"), std::string::npos);
}

TEST_F(DependencyPrinterTest, ThreeLevelHierarchy) {
  SymbolTable root(ScopingMode::Local);
  root.define("global_param", arena.make<NumberLiteral>(1.0));

  auto level1 = root.create_child();
  level1->define("level1_param", arena.make<Identifier>("global_param"));

  auto level2 = level1->create_child();
  level2->define("level2_param", arena.make<Identifier>("level1_param"));

  ScopeHierarchy hierarchy{
      &root,
      "root",
      {{level1.get(), "subckt1", {{level2.get(), "nested", {}}}}}};

  DependencyPrinter printer;
  std::string output = printer.print(hierarchy);

  EXPECT_NE(output.find("Scope: root"), std::string::npos);
  EXPECT_NE(output.find("Scope: root/subckt1"), std::string::npos);
  EXPECT_NE(output.find("Scope: root/subckt1/nested"), std::string::npos);
}

TEST_F(DependencyPrinterTest, LocalScopeShadowing) {
  SymbolTable root(ScopingMode::Local);
  root.define("param", arena.make<NumberLiteral>(1.0));

  auto child = root.create_child();
  // Child shadows parent's param
  child->define("param", arena.make<NumberLiteral>(2.0));
  child->define("other", arena.make<Identifier>("param"));

  ScopeHierarchy hierarchy{&root, "root", {{child.get(), "child", {}}}};

  DependencyPrinter printer;
  std::string output = printer.print(hierarchy);

  // The dependency should resolve to child's param, not root's
  EXPECT_NE(output.find("depends on: param [root/child]"), std::string::npos);
}

TEST_F(DependencyPrinterTest, HideExpressions) {
  SymbolTable symbols;
  symbols.define("vdd", arena.make<NumberLiteral>(1.8));

  DependencyPrinter printer;
  printer.set_show_expressions(false);
  std::string output = printer.print(symbols, "root");

  // Should have the parameter name but not the value
  EXPECT_NE(output.find("vdd"), std::string::npos);
  EXPECT_EQ(output.find("1.8"), std::string::npos);
}

TEST_F(DependencyPrinterTest, ShowExpressions) {
  SymbolTable symbols;
  symbols.define("vdd", arena.make<NumberLiteral>(1.8));

  DependencyPrinter printer;
  printer.set_show_expressions(true);
  std::string output = printer.print(symbols, "root");

  EXPECT_NE(output.find("vdd = 1.8"), std::string::npos);
}

TEST_F(DependencyPrinterTest, MultipleDependencies) {
  SymbolTable symbols;
  symbols.define("a", arena.make<NumberLiteral>(1.0));
  symbols.define("b", arena.make<NumberLiteral>(2.0));
  symbols.define("c", arena.make<BinaryOp>(BinaryOpType::Add, arena.make<Identifier>("a"),
                                           arena.make<Identifier>("b")));

  DependencyPrinter printer;
  std::string output = printer.print(symbols, "root");

  EXPECT_NE(output.find("depends on: a"), std::string::npos);
  EXPECT_NE(output.find("depends on: b"), std::string::npos);
}

TEST_F(DependencyPrinterTest, ChainedDependencies) {
  SymbolTable symbols;
  symbols.define("a", arena.make<NumberLiteral>(1.0));
  symbols.define("b", arena.make<Identifier>("a"));
  symbols.define("c", arena.make<Identifier>("b"));

  DependencyPrinter printer;
  std::string output = printer.print(symbols, "root");

  // b depends on a
  EXPECT_NE(output.find("b"), std::string::npos);
  // c depends on b
  EXPECT_NE(output.find("c"), std::string::npos);
}

TEST_F(DependencyPrinterTest, PrintToStream) {
  SymbolTable symbols;
  symbols.define("x", arena.make<NumberLiteral>(42.0));

  ScopeHierarchy hierarchy{&symbols, "test", {}};

  DependencyPrinter printer;
  std::ostringstream ss;
  printer.print(hierarchy, ss);

  std::string output = ss.str();
  EXPECT_NE(output.find("Scope: test"), std::string::npos);
  EXPECT_NE(output.find("x"), std::string::npos);
}
