#include <gtest/gtest.h>

#include <cmath>

#include "spice_expr/spice_expr.h"

using namespace spice_expr;

// =============================================================================
// ADS Gummel-Poon BJT Model Tests
// =============================================================================
// Verifies that the translated ADS SDD-based Gummel-Poon netlist expressions
// parse and evaluate correctly in spice_expr. Expressions are translated from
// ADS syntax (^ -> **, if/then/else/endif -> ?:, drop unit suffixes).

class ADSGummelPoonTest : public ::testing::Test {
 protected:
  ExpressionEngine engine;
  ExprArena& arena = engine.arena();

  // Helper to parse and evaluate
  double eval(const std::string& expr_str) {
    auto* expr = parse_expression(expr_str, arena);
    return engine.evaluate_real(*expr);
  }

  // Helper to define a parameter with a numeric value
  void param(const std::string& name, double value) {
    engine.define_parameter(name, arena.make<NumberLiteral>(value));
  }

  // Helper to define a parameter with an expression string
  void param_expr(const std::string& name, const std::string& expr_str) {
    engine.define_parameter(name, parse_expression(expr_str, arena));
  }

  // Helper to define a user function from a string body
  void def_func(const std::string& name, const std::vector<std::string>& params,
                const std::string& body_str) {
    auto* body = parse_expression(body_str, arena);
    engine.define_function(name, params, body);
  }
};

// -----------------------------------------------------------------------------
// Test 1: Predefined constants parse and evaluate
// -----------------------------------------------------------------------------
TEST_F(ADSGummelPoonTest, PredefinedConstants) {
  param("pi", 3.14159265358979323846);
  param("boltzmann", 1.380649e-23);
  param("qelectron", 1.602176634e-19);

  EXPECT_NEAR(eval("pi"), M_PI, 1e-15);
  EXPECT_NEAR(eval("boltzmann"), 1.380649e-23, 1e-38);
  EXPECT_NEAR(eval("qelectron"), 1.602176634e-19, 1e-34);
}

// -----------------------------------------------------------------------------
// Test 2: Thermal voltage vt = boltzmann * (Tamb + TzeroC) / qelectron
// -----------------------------------------------------------------------------
TEST_F(ADSGummelPoonTest, ThermalVoltage) {
  param("boltzmann", 1.380649e-23);
  param("qelectron", 1.602176634e-19);
  param("Tamb", 27.0);
  param("TzeroC", 273.15);

  double result = eval("boltzmann * (Tamb + TzeroC) / qelectron");

  double expected = 1.380649e-23 * (27.0 + 273.15) / 1.602176634e-19;
  EXPECT_NEAR(result, expected, 1e-10);
}

// -----------------------------------------------------------------------------
// Test 3: User-defined exp_soft with ternary (translated from if/then/else)
// -----------------------------------------------------------------------------
TEST_F(ADSGummelPoonTest, ExpSoftFunction) {
  param("exp_max", std::log(1e16));

  def_func("exp_soft", {"x"},
           "(x < exp_max) ? exp(x) : (x + 1 - exp_max) * exp(exp_max)");

  // Normal range: should return exp(x)
  double result_normal = eval("exp_soft(5.0)");
  EXPECT_NEAR(result_normal, std::exp(5.0), 1e-10);

  // Beyond exp_max: should use linear extrapolation
  double exp_max = std::log(1e16);
  double x_large = exp_max + 10.0;
  param("x_large", x_large);
  double result_large = eval("exp_soft(x_large)");
  double expected_large = (x_large + 1 - exp_max) * std::exp(exp_max);
  EXPECT_NEAR(result_large, expected_large, 1e-2);
}

// -----------------------------------------------------------------------------
// Test 4: User-defined Eg(t) bandgap energy
// -----------------------------------------------------------------------------
TEST_F(ADSGummelPoonTest, BandgapEnergyFunction) {
  param("eg0", 1.16);
  param("TzeroC", 273.15);

  def_func("Eg", {"t"}, "eg0 - 7.02e-4 * (t + TzeroC)**2 / (1108 + (t + TzeroC))");

  double Tamb = 27.0;
  param("Tamb", Tamb);
  double result = eval("Eg(Tamb)");

  double T_abs = Tamb + 273.15;
  double expected = 1.16 - 7.02e-4 * T_abs * T_abs / (1108.0 + T_abs);
  EXPECT_NEAR(result, expected, 1e-10);
}

// -----------------------------------------------------------------------------
// Test 5: User-defined diode(v, is, n) calling exp_soft
// -----------------------------------------------------------------------------
TEST_F(ADSGummelPoonTest, DiodeFunction) {
  param("boltzmann", 1.380649e-23);
  param("qelectron", 1.602176634e-19);
  param("Tamb", 27.0);
  param("TzeroC", 273.15);
  param_expr("vt", "boltzmann * (Tamb + TzeroC) / qelectron");
  param("exp_max", std::log(1e16));

  def_func("exp_soft", {"x"},
           "(x < exp_max) ? exp(x) : (x + 1 - exp_max) * exp(exp_max)");
  def_func("diode", {"v", "is", "n"}, "is * (exp_soft(v / (n * vt)) - 1)");

  engine.build_evaluation_order();

  // Forward-biased diode
  double vt = 1.380649e-23 * 300.15 / 1.602176634e-19;
  double Is = 1e-15;
  double nf = 1.0;
  double Vf = 0.7;
  param("Is_test", Is);
  param("Vf_test", Vf);
  double result = eval("diode(Vf_test, Is_test, 1.0)");
  double expected = Is * (std::exp(Vf / (nf * vt)) - 1);
  EXPECT_NEAR(result, expected, expected * 1e-6);  // relative tolerance
}

// -----------------------------------------------------------------------------
// Test 6: Full Gummel-Poon parameter chain
// -----------------------------------------------------------------------------
TEST_F(ADSGummelPoonTest, FullParameterChain) {
  // Predefined constants
  param("pi", 3.14159265358979323846);
  param("boltzmann", 1.380649e-23);
  param("qelectron", 1.602176634e-19);

  // Model parameters (typical NPN BJT)
  param("Tamb", 27.0);
  param("TzeroC", 273.15);
  param("eg0", 1.16);
  param("bf", 100.0);
  param("br", 1.0);
  param("js", 1e-15);
  param("nf", 1.0);
  param("nr", 1.0);
  param("ne", 1.5);
  param("nc", 2.0);
  param("jle", 0.0);
  param("jlc", 0.0);
  param("jbf", 1e-2);
  param("jbr", 1e-2);
  param("rb", 100.0);
  param("rbm", 10.0);
  param("rc", 10.0);
  param("re", 1.0);
  param("jrb", 1e-3);
  param("cje", 1e-12);
  param("vje", 0.75);
  param("mje", 0.33);
  param("cjc", 0.5e-12);
  param("vjc", 0.75);
  param("mjc", 0.33);
  param("tf", 1e-11);
  param("tr", 1e-8);
  param("xtf", 0.0);
  param("vtf", 1e10);
  param("jtf", 1e10);
  param("xti", 3.0);
  param("vbf", 100.0);
  param("vbr", 100.0);

  // User-defined functions
  param_expr("exp_max", "ln(1e16)");
  def_func("exp_soft", {"x"},
           "(x < exp_max) ? exp(x) : (x + 1 - exp_max) * exp(exp_max)");
  def_func("Eg", {"t"}, "eg0 - 7.02e-4 * (t + TzeroC)**2 / (1108 + (t + TzeroC))");
  def_func("diode", {"v", "is", "n"}, "is * (exp_soft(v / (n * vt)) - 1)");

  // Derived parameters
  param_expr("Tj", "Tamb");
  param_expr("vt", "boltzmann * (Tamb + TzeroC) / qelectron");
  param_expr("vjc_T", "vjc");
  param_expr("vje_T", "vje");
  param_expr("Bf_T", "bf");
  param_expr("Br_T", "br");
  param_expr("TempRatio", "(Tj + TzeroC) / (Tamb + TzeroC)");
  param_expr("Js_T",
             "js * TempRatio**xti * exp_soft(Eg(Tamb) * ((Tj - Tamb) / ((Tamb + TzeroC) * vt)))");

  // Bias point: Vbe=0.7V, Vce=2.0V (active forward mode)
  // Vce = Vc - Ve, so Vc = Ve + Vce = 0 + 2.0 = 2.0
  param("_v1", 0.7);   // vb
  param("_v2", 2.0);   // vc
  param("_v3", 0.0);   // ve
  param("_v4", 0.7);   // vbi (approx, ignoring Rbb drop)
  param("_v5", 2.0);   // vci (approx, ignoring rc drop)
  param("_v6", 0.0);   // vei (approx, ignoring re drop)

  param_expr("vb", "_v1");
  param_expr("vc", "_v2");
  param_expr("ve", "_v3");
  param_expr("vbi", "_v4");
  param_expr("vci", "_v5");
  param_expr("vei", "_v6");
  param_expr("vbei", "vbi - vei");
  param_expr("vbci", "vbi - vci");
  param_expr("vbe", "vb - ve");
  param_expr("vbc", "vb - vc");

  // Q1, Q2, QB
  param_expr("Q1", "1 / (1 - vbci/vbf - vbei/vbr)");
  param_expr("Q2", "diode(vbei, Js_T/jbf, nf) + diode(vbci, Js_T/jbr, nr)");
  param_expr("QB", "(Q1/2) * (1 + sqrt(1 + 4*Q2))");

  // Currents
  param_expr("iff", "diode(vbei, Js_T, nf)");
  param_expr("TFF", "tf * (1 + xtf * exp(vbci/(1.44*vtf) * (iff/(iff+jtf))**2))");

  // Charge terms
  param_expr("qbc",
             "tr*diode(vbci, Js_T, nr) - cjc*vjc_T*((1 - vbci/vjc_T)**(1-mjc)) / (1-mjc)");
  param_expr("qbe",
             "diode(vbei, TFF*js/QB, nf) - cje*vje_T*((1 - vbei/vje_T)**(1-mje)) / (1-mje)");
  param_expr("qb2", "qbc");

  // Terminal currents
  param_expr("ib1", "diode(vbei, Js_T/Bf_T, nf) + diode(vbci, Js_T/Br_T, nr)");
  param_expr("ib2", "diode(vbei, jle, ne) + diode(vbci, jlc, nc)");
  param_expr("ic1", "diode(vbei, Js_T/QB, nf) - diode(vbci, js/QB, nr)");
  param_expr("ic2", "-diode(vbci, Js_T/Br_T, nr) - diode(vbci, jlc, nc)");
  param_expr("ib", "ib1 + ib2");
  param_expr("ic", "ic1 + ic2");
  param_expr("ie", "-(ib + ic)");

  // Base resistance
  param_expr("ibb", "(ib < 1p) ? 1p : ib");
  param_expr("Z",
             "(-1 + sqrt(144*ibb/(pi**2*jrb) + 1)) / ((24/pi**2) * sqrt(ibb/jrb))");
  param_expr("Rbb", "3*(rb - rbm)*(tan(Z) - Z) / (Z * tan(Z)**2) + rbm");

  engine.build_evaluation_order();
  EXPECT_TRUE(engine.has_valid_evaluation_order());
  EXPECT_FALSE(engine.has_cyclic_dependencies());

  // --- Verify key intermediate and final values ---

  // vt should be ~25.9mV at 27C
  double vt = engine.evaluate_real(*arena.make<Identifier>("vt"));
  double expected_vt = 1.380649e-23 * 300.15 / 1.602176634e-19;
  EXPECT_NEAR(vt, expected_vt, 1e-10);

  // Js_T: at Tj=Tamb, TempRatio=1, Eg term exponent=0 => Js_T = js * 1 * 1 = js
  double Js_T = engine.evaluate_real(*arena.make<Identifier>("Js_T"));
  EXPECT_NEAR(Js_T, 1e-15, 1e-25);

  // vbei = 0.7, vbci = 0.7 - 2.0 = -1.3 (reverse biased in active mode)
  double vbei = engine.evaluate_real(*arena.make<Identifier>("vbei"));
  EXPECT_NEAR(vbei, 0.7, 1e-10);
  double vbci = engine.evaluate_real(*arena.make<Identifier>("vbci"));
  EXPECT_NEAR(vbci, -1.3, 1e-10);

  // Forward diode current: diode(0.7, 1e-15, 1.0) should be large
  double iff_val = engine.evaluate_real(*arena.make<Identifier>("iff"));
  double expected_iff = 1e-15 * (std::exp(0.7 / vt) - 1);
  EXPECT_NEAR(iff_val, expected_iff, expected_iff * 1e-6);

  // Collector current should be positive and significant
  double ic_val = engine.evaluate_real(*arena.make<Identifier>("ic"));
  EXPECT_GT(ic_val, 0.0);

  // Base current should be positive
  double ib_val = engine.evaluate_real(*arena.make<Identifier>("ib"));
  EXPECT_GT(ib_val, 0.0);

  // Current gain: ic/ib should be approximately bf (=100) for this bias
  double beta = ic_val / ib_val;
  EXPECT_GT(beta, 50.0);
  EXPECT_LT(beta, 200.0);

  // KCL: ie = -(ib + ic)
  double ie_val = engine.evaluate_real(*arena.make<Identifier>("ie"));
  EXPECT_NEAR(ie_val, -(ib_val + ic_val), std::abs(ie_val) * 1e-10);
}

// -----------------------------------------------------------------------------
// Test 7: Verify individual translated expressions parse correctly
// -----------------------------------------------------------------------------
TEST_F(ADSGummelPoonTest, AllExpressionsParse) {
  // Set up minimal symbol table so parsing succeeds
  // (parsing doesn't need values, but evaluation would)
  std::vector<std::string> expressions = {
      "boltzmann * (Tamb + TzeroC) / qelectron",
      "ln(1e16)",
      "(Tj + TzeroC) / (Tamb + TzeroC)",
      "js * TempRatio**xti * exp_soft(Eg(Tamb) * ((Tj - Tamb) / ((Tamb + TzeroC) * vt)))",
      "1 / (1 - vbci/vbf - vbei/vbr)",
      "(Q1/2) * (1 + sqrt(1 + 4*Q2))",
      "(ib < 1p) ? 1p : ib",
      "(-1 + sqrt(144*ibb/(pi**2*jrb) + 1)) / ((24/pi**2) * sqrt(ibb/jrb))",
      "3*(rb - rbm)*(tan(Z) - Z) / (Z * tan(Z)**2) + rbm",
      "tf * (1 + xtf * exp(vbci/(1.44*vtf) * (iff/(iff+jtf))**2))",
      "tr*diode(vbci, Js_T, nr) - cjc*vjc_T*((1 - vbci/vjc_T)**(1-mjc)) / (1-mjc)",
      "diode(vbei, TFF*js/QB, nf) - cje*vje_T*((1 - vbei/vje_T)**(1-mje)) / (1-mje)",
      "diode(vbei, Js_T/Bf_T, nf) + diode(vbci, Js_T/Br_T, nr)",
      "diode(vbei, jle, ne) + diode(vbci, jlc, nc)",
      "diode(vbei, Js_T/QB, nf) - diode(vbci, js/QB, nr)",
      "-diode(vbci, Js_T/Br_T, nr) - diode(vbci, jlc, nc)",
      "ib1 + ib2",
      "ic1 + ic2",
      "-(ib + ic)",
      "eg0 - 7.02e-4 * (t + TzeroC)**2 / (1108 + (t + TzeroC))",
      "is * (exp_soft(v / (n * vt)) - 1)",
      "(x < exp_max) ? exp(x) : (x + 1 - exp_max) * exp(exp_max)",
  };

  for (const auto& expr_str : expressions) {
    EXPECT_NO_THROW(parse_expression(expr_str, arena))
        << "Failed to parse: " << expr_str;
  }
}

// -----------------------------------------------------------------------------
// Test 8: Engineering notation 1p (pico) parses correctly
// -----------------------------------------------------------------------------
TEST_F(ADSGummelPoonTest, EngineeringNotationPico) {
  // "1p" should parse as 1e-12
  double result = eval("1p");
  EXPECT_NEAR(result, 1e-12, 1e-25);

  // Used in ternary: (x < 1p) ? 1p : x
  param("x_small", 0.5e-12);
  param("x_large", 5e-12);

  double r1 = eval("(x_small < 1p) ? 1p : x_small");
  EXPECT_NEAR(r1, 1e-12, 1e-25);

  double r2 = eval("(x_large < 1p) ? 1p : x_large");
  EXPECT_NEAR(r2, 5e-12, 1e-25);
}

// -----------------------------------------------------------------------------
// Test 9: Power operator ** with fractional exponents
// -----------------------------------------------------------------------------
TEST_F(ADSGummelPoonTest, PowerOperatorFractional) {
  param("vjc_T", 0.75);
  param("mjc", 0.33);
  param("vbci", 0.3);

  // (1 - vbci/vjc_T)**(1-mjc) — junction capacitance depletion term
  double result = eval("(1 - vbci/vjc_T)**(1 - mjc)");
  double expected = std::pow(1.0 - 0.3 / 0.75, 1.0 - 0.33);
  EXPECT_NEAR(result, expected, 1e-10);
}

// -----------------------------------------------------------------------------
// Test 10: Base resistance Rbb with tan() and nested sqrt
// -----------------------------------------------------------------------------
TEST_F(ADSGummelPoonTest, BaseResistanceRbb) {
  param("pi", 3.14159265358979323846);
  param("rb", 100.0);
  param("rbm", 10.0);
  param("jrb", 1e-3);
  param("ibb", 1e-4);

  // Z calculation
  double pi = M_PI;
  double ibb = 1e-4;
  double jrb = 1e-3;
  double expected_Z =
      (-1 + std::sqrt(144 * ibb / (pi * pi * jrb) + 1)) /
      ((24.0 / (pi * pi)) * std::sqrt(ibb / jrb));

  double result_Z = eval(
      "(-1 + sqrt(144*ibb/(pi**2*jrb) + 1)) / ((24/pi**2) * sqrt(ibb/jrb))");
  EXPECT_NEAR(result_Z, expected_Z, 1e-10);

  // Rbb calculation
  param("Z", expected_Z);
  double Z = expected_Z;
  double expected_Rbb =
      3.0 * (100.0 - 10.0) * (std::tan(Z) - Z) /
      (Z * std::tan(Z) * std::tan(Z)) + 10.0;

  double result_Rbb =
      eval("3*(rb - rbm)*(tan(Z) - Z) / (Z * tan(Z)**2) + rbm");
  EXPECT_NEAR(result_Rbb, expected_Rbb, 1e-8);
}

// -----------------------------------------------------------------------------
// Test 11: Nested user-defined function calls (diode calls exp_soft)
// -----------------------------------------------------------------------------
TEST_F(ADSGummelPoonTest, NestedUserFunctions) {
  param("boltzmann", 1.380649e-23);
  param("qelectron", 1.602176634e-19);
  param("Tamb", 27.0);
  param("TzeroC", 273.15);
  param_expr("vt", "boltzmann * (Tamb + TzeroC) / qelectron");
  param("exp_max", std::log(1e16));

  def_func("exp_soft", {"x"},
           "(x < exp_max) ? exp(x) : (x + 1 - exp_max) * exp(exp_max)");
  def_func("diode", {"v", "is", "n"}, "is * (exp_soft(v / (n * vt)) - 1)");

  engine.build_evaluation_order();

  double vt = 1.380649e-23 * 300.15 / 1.602176634e-19;

  // Forward bias
  double result_fwd = eval("diode(0.7, 1e-15, 1.0)");
  double expected_fwd = 1e-15 * (std::exp(0.7 / vt) - 1);
  EXPECT_NEAR(result_fwd, expected_fwd, expected_fwd * 1e-6);

  // Reverse bias: diode(-0.5, 1e-15, 1.0) should be approximately -Is
  double result_rev = eval("diode(-0.5, 1e-15, 1.0)");
  double expected_rev = 1e-15 * (std::exp(-0.5 / vt) - 1);
  EXPECT_NEAR(result_rev, expected_rev, 1e-16);

  // Zero bias: diode(0, 1e-15, 1.0) should be 0
  double result_zero = eval("diode(0.0, 1e-15, 1.0)");
  EXPECT_NEAR(result_zero, 0.0, 1e-20);
}

// -----------------------------------------------------------------------------
// Test 12: Junction charge qbc and qbe expressions
// -----------------------------------------------------------------------------
TEST_F(ADSGummelPoonTest, JunctionChargeExpressions) {
  param("boltzmann", 1.380649e-23);
  param("qelectron", 1.602176634e-19);
  param("Tamb", 27.0);
  param("TzeroC", 273.15);
  param_expr("vt", "boltzmann * (Tamb + TzeroC) / qelectron");
  param("exp_max", std::log(1e16));

  def_func("exp_soft", {"x"},
           "(x < exp_max) ? exp(x) : (x + 1 - exp_max) * exp(exp_max)");
  def_func("diode", {"v", "is", "n"}, "is * (exp_soft(v / (n * vt)) - 1)");

  param("Js_T", 1e-15);
  param("nr", 1.0);
  param("tr", 1e-8);
  param("cjc", 0.5e-12);
  param("vjc_T", 0.75);
  param("mjc", 0.33);
  param("vbci", -1.3);  // reverse biased BC junction

  engine.build_evaluation_order();

  double vt = 1.380649e-23 * 300.15 / 1.602176634e-19;
  double vbci = -1.3;

  // qbc = tr*diode(vbci, Js_T, nr) - cjc*vjc_T*((1-vbci/vjc_T)**(1-mjc))/(1-mjc)
  double result_qbc = eval(
      "tr*diode(vbci, Js_T, nr) - cjc*vjc_T*((1 - vbci/vjc_T)**(1-mjc)) / (1-mjc)");

  double diode_bc = 1e-15 * (std::exp(vbci / (1.0 * vt)) - 1);
  double depl_bc = 0.5e-12 * 0.75 *
                   std::pow(1.0 - vbci / 0.75, 1.0 - 0.33) / (1.0 - 0.33);
  double expected_qbc = 1e-8 * diode_bc - depl_bc;
  EXPECT_NEAR(result_qbc, expected_qbc, std::abs(expected_qbc) * 1e-6);
}
