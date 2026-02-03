#include <gtest/gtest.h>

#include "spice_expr/spice_expr.h"

using namespace spice_expr;

class SymbolTableTest : public ::testing::Test {
 protected:
  ExprArena arena;
};

TEST_F(SymbolTableTest, DefineAndLookup) {
  SymbolTable table;
  table.define("vdd", arena.make<NumberLiteral>(1.8));

  const SymbolEntry* entry = table.lookup("vdd");
  ASSERT_NE(entry, nullptr);
  EXPECT_EQ(entry->name, "vdd");
  EXPECT_NE(entry->expression, nullptr);
}

TEST_F(SymbolTableTest, LookupNonexistent) {
  SymbolTable table;
  const SymbolEntry* entry = table.lookup("nonexistent");
  EXPECT_EQ(entry, nullptr);
}

TEST_F(SymbolTableTest, Exists) {
  SymbolTable table;
  EXPECT_FALSE(table.exists("vdd"));
  table.define("vdd", arena.make<NumberLiteral>(1.8));
  EXPECT_TRUE(table.exists("vdd"));
}

TEST_F(SymbolTableTest, Override) {
  SymbolTable table;
  table.define("vdd", arena.make<NumberLiteral>(1.8));
  table.set_override("vdd", arena.make<NumberLiteral>(3.3));

  const SymbolEntry* entry = table.lookup("vdd");
  ASSERT_NE(entry, nullptr);
  ASSERT_NE(entry->expression, nullptr);
  EXPECT_EQ(static_cast<NumberLiteral*>(entry->expression)->real(), 3.3);
}

TEST_F(SymbolTableTest, ClearOverride) {
  SymbolTable table;
  auto* original = arena.make<NumberLiteral>(1.8);
  table.define("vdd", original);
  table.set_override("vdd", arena.make<NumberLiteral>(3.3));
  table.clear_override("vdd");

  const SymbolEntry* entry = table.lookup("vdd");
  ASSERT_NE(entry, nullptr);
  EXPECT_EQ(entry->expression, original);
}

TEST_F(SymbolTableTest, ClearAllOverrides) {
  SymbolTable table;
  table.define("vdd", arena.make<NumberLiteral>(1.8));
  table.define("vss", arena.make<NumberLiteral>(0.0));
  table.set_override("vdd", arena.make<NumberLiteral>(3.3));
  table.set_override("vss", arena.make<NumberLiteral>(-1.0));
  table.clear_all_overrides();

  EXPECT_EQ(static_cast<NumberLiteral*>(table.lookup("vdd")->expression)->real(), 1.8);
  EXPECT_EQ(static_cast<NumberLiteral*>(table.lookup("vss")->expression)->real(), 0.0);
}

TEST_F(SymbolTableTest, GlobalScopingLooksUpRootFirst) {
  SymbolTable root(ScopingMode::Global);
  root.define("vdd", arena.make<NumberLiteral>(1.8));

  auto child = root.create_child();
  child->define("vdd", arena.make<NumberLiteral>(3.3));

  // In global scoping mode, root symbols take precedence
  const SymbolEntry* entry = child->lookup("vdd");
  ASSERT_NE(entry, nullptr);
  EXPECT_EQ(static_cast<NumberLiteral*>(entry->expression)->real(), 1.8);
}

TEST_F(SymbolTableTest, LocalScopingMode) {
  SymbolTable parent(ScopingMode::Local);
  parent.define("vdd", arena.make<NumberLiteral>(1.8));

  auto child = parent.create_child();
  child->define("vth", arena.make<NumberLiteral>(0.4));

  EXPECT_NE(child->lookup("vdd"), nullptr);
  EXPECT_NE(child->lookup("vth"), nullptr);
  EXPECT_EQ(parent.lookup("vth"), nullptr);
}

TEST_F(SymbolTableTest, ChildShadowsParent) {
  SymbolTable parent(ScopingMode::Local);
  parent.define("vdd", arena.make<NumberLiteral>(1.8));

  auto child = parent.create_child();
  child->define("vdd", arena.make<NumberLiteral>(3.3));

  const SymbolEntry* parentEntry = parent.lookup("vdd");
  const SymbolEntry* childEntry = child->lookup("vdd");

  EXPECT_EQ(static_cast<NumberLiteral*>(parentEntry->expression)->real(), 1.8);
  EXPECT_EQ(static_cast<NumberLiteral*>(childEntry->expression)->real(), 3.3);
}

TEST_F(SymbolTableTest, AllSymbolNames) {
  SymbolTable table;
  table.define("a", arena.make<NumberLiteral>(1.0));
  table.define("b", arena.make<NumberLiteral>(2.0));
  table.define("c", arena.make<NumberLiteral>(3.0));

  auto names = table.all_symbol_names();
  EXPECT_EQ(names.size(), 3);
  EXPECT_TRUE(std::find(names.begin(), names.end(), "a") != names.end());
  EXPECT_TRUE(std::find(names.begin(), names.end(), "b") != names.end());
  EXPECT_TRUE(std::find(names.begin(), names.end(), "c") != names.end());
}

TEST_F(SymbolTableTest, Redefine) {
  SymbolTable table;
  table.define("param", arena.make<NumberLiteral>(1.0));
  table.define("param", arena.make<NumberLiteral>(2.0));

  const SymbolEntry* entry = table.lookup("param");
  EXPECT_EQ(static_cast<NumberLiteral*>(entry->expression)->real(), 2.0);
}

TEST_F(SymbolTableTest, ScopingModeGetter) {
  SymbolTable localTable(ScopingMode::Local);
  SymbolTable globalTable(ScopingMode::Global);

  EXPECT_EQ(localTable.scoping_mode(), ScopingMode::Local);
  EXPECT_EQ(globalTable.scoping_mode(), ScopingMode::Global);
}

TEST_F(SymbolTableTest, ParentPointer) {
  SymbolTable parent;
  auto child = parent.create_child();

  EXPECT_EQ(parent.parent(), nullptr);
  EXPECT_EQ(child->parent(), &parent);
}

TEST_F(SymbolTableTest, GlobalScopingRootOverride) {
  SymbolTable root(ScopingMode::Global);
  root.define("vdd", arena.make<NumberLiteral>(1.8));
  root.set_override("vdd", arena.make<NumberLiteral>(3.3));

  auto child = root.create_child();

  // Root's override should be applied when looking up through child
  const SymbolEntry* entry = child->lookup("vdd");
  ASSERT_NE(entry, nullptr);
  EXPECT_EQ(static_cast<NumberLiteral*>(entry->expression)->real(), 3.3);
}

TEST_F(SymbolTableTest, GlobalScopingChildOverridesRootSymbol) {
  SymbolTable root(ScopingMode::Global);
  root.define("vdd", arena.make<NumberLiteral>(1.8));

  auto child = root.create_child();
  child->set_override("vdd", arena.make<NumberLiteral>(5.0));

  // Child's override on root symbol should take effect
  const SymbolEntry* entry = child->lookup("vdd");
  ASSERT_NE(entry, nullptr);
  EXPECT_EQ(static_cast<NumberLiteral*>(entry->expression)->real(), 5.0);
}

TEST_F(SymbolTableTest, SetAndHasFlag) {
  SymbolTable table;
  table.define("param", arena.make<NumberLiteral>(1.0));

  EXPECT_FALSE(table.has_flag("param", "optimize"));
  table.set_flag("param", "optimize");
  EXPECT_TRUE(table.has_flag("param", "optimize"));
}

TEST_F(SymbolTableTest, SetMultipleFlags) {
  SymbolTable table;
  table.define("param", arena.make<NumberLiteral>(1.0));

  table.set_flag("param", "optimize");
  table.set_flag("param", "sweep");

  EXPECT_TRUE(table.has_flag("param", "optimize"));
  EXPECT_TRUE(table.has_flag("param", "sweep"));
}

TEST_F(SymbolTableTest, ClearFlag) {
  SymbolTable table;
  table.define("param", arena.make<NumberLiteral>(1.0));

  table.set_flag("param", "optimize");
  table.set_flag("param", "sweep");
  table.clear_flag("param", "optimize");

  EXPECT_FALSE(table.has_flag("param", "optimize"));
  EXPECT_TRUE(table.has_flag("param", "sweep"));
}

TEST_F(SymbolTableTest, ClearAllFlags) {
  SymbolTable table;
  table.define("param", arena.make<NumberLiteral>(1.0));

  table.set_flag("param", "optimize");
  table.set_flag("param", "sweep");
  table.clear_all_flags("param");

  EXPECT_FALSE(table.has_flag("param", "optimize"));
  EXPECT_FALSE(table.has_flag("param", "sweep"));
}

TEST_F(SymbolTableTest, GetFlags) {
  SymbolTable table;
  table.define("param", arena.make<NumberLiteral>(1.0));

  table.set_flag("param", "optimize");
  table.set_flag("param", "sweep");

  auto flags = table.get_flags("param");
  EXPECT_EQ(flags.size(), 2);
  EXPECT_TRUE(flags.count("optimize") > 0);
  EXPECT_TRUE(flags.count("sweep") > 0);
}

TEST_F(SymbolTableTest, GetFlagsNonexistent) {
  SymbolTable table;
  auto flags = table.get_flags("nonexistent");
  EXPECT_TRUE(flags.empty());
}

TEST_F(SymbolTableTest, GetParametersWithFlag) {
  SymbolTable table;
  auto* exprA = arena.make<NumberLiteral>(1.0);
  auto* exprB = arena.make<NumberLiteral>(2.0);
  auto* exprC = arena.make<NumberLiteral>(3.0);
  table.define("a", exprA);
  table.define("b", exprB);
  table.define("c", exprC);

  table.set_flag("a", "optimize");
  table.set_flag("b", "optimize");
  table.set_flag("c", "sweep");

  auto optimizeParams = table.get_parameters_with_flag("optimize");
  EXPECT_EQ(optimizeParams.size(), 2);
  EXPECT_TRUE(std::find(optimizeParams.begin(), optimizeParams.end(), exprA) !=
              optimizeParams.end());
  EXPECT_TRUE(std::find(optimizeParams.begin(), optimizeParams.end(), exprB) !=
              optimizeParams.end());

  auto sweepParams = table.get_parameters_with_flag("sweep");
  EXPECT_EQ(sweepParams.size(), 1);
  EXPECT_TRUE(std::find(sweepParams.begin(), sweepParams.end(), exprC) != sweepParams.end());
}

TEST_F(SymbolTableTest, FlagOnNonexistentSymbol) {
  SymbolTable table;
  // Should not crash when setting flag on nonexistent symbol
  table.set_flag("nonexistent", "flag");
  EXPECT_FALSE(table.has_flag("nonexistent", "flag"));
}
