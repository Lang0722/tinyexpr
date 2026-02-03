#include <gtest/gtest.h>

#include <cmath>

#include "spice_expr/spice_expr.h"

using namespace spice_expr;

// =============================================================================
// Compact Model Tests
// =============================================================================
// These tests evaluate expressions typical of compact semiconductor models
// (EKV MOSFET, HICUM BJT, BSIM) to verify the expression evaluator handles
// deeply nested functions and realistic device physics equations.

class CompactModelTest : public ::testing::Test {
 protected:
  ExpressionEngine engine;
  MockCircuitInterface circuit;

  // Helper to parse and evaluate in one step
  double eval(const std::string& expr_str) {
    ExprArena& arena = engine.arena();
    auto* expr = parse_expression(expr_str, arena);
    return engine.evaluate_real(*expr, circuit);
  }

  // Helper to define a parameter with a numeric value
  void param(const std::string& name, double value) {
    ExprArena& arena = engine.arena();
    engine.define_parameter(name, arena.make<NumberLiteral>(value));
  }
};

// =============================================================================
// EKV Model Style Tests
// =============================================================================

TEST_F(CompactModelTest, BandgapEnergy) {
  // Eg = 1.16 - 7.02e-4 * T * T / (T + 1108.0)
  // Silicon bandgap energy as function of temperature
  param("T", 300.0);

  double result = eval("1.16 - 7.02e-4 * T * T / (T + 1108.0)");

  // Expected: 1.16 - 7.02e-4 * 90000 / 1408 = 1.16 - 0.04486 = 1.1151
  double expected = 1.16 - 7.02e-4 * 300.0 * 300.0 / (300.0 + 1108.0);
  EXPECT_NEAR(result, expected, 1e-10);
}

TEST_F(CompactModelTest, BuiltinPotentialThermalVoltage) {
  // PHI_T = PHI * ratioT - 3.0 * Vt * ln(ratioT) - refEg * ratioT + Eg
  // Temperature-scaled built-in potential
  param("PHI", 0.7);
  param("ratioT", 1.1);  // T/Tnom = 330/300
  param("Vt", 0.0285);   // kT/q at 330K
  param("refEg", 1.12);
  param("Eg", 1.10);

  double result = eval("PHI * ratioT - 3.0 * Vt * ln(ratioT) - refEg * ratioT + Eg");

  double ratioT = 1.1;
  double expected = 0.7 * ratioT - 3.0 * 0.0285 * std::log(ratioT) - 1.12 * ratioT + 1.10;
  EXPECT_NEAR(result, expected, 1e-10);
}

TEST_F(CompactModelTest, PinchoffVoltageNestedSqrt) {
  // VP = VGprime - PHI_T - GAMMA * (sqrt(VGprime + 0.25 * GAMMA^2) - 0.5 * GAMMA)
  // EKV pinch-off voltage calculation with nested square roots
  param("VGprime", 1.5);
  param("PHI_T", 0.65);
  param("GAMMA", 0.5);

  double result = eval("VGprime - PHI_T - GAMMA * (sqrt(VGprime + 0.25 * GAMMA**2) - 0.5 * GAMMA)");

  double VGprime = 1.5, PHI_T = 0.65, GAMMA = 0.5;
  double expected = VGprime - PHI_T - GAMMA * (std::sqrt(VGprime + 0.25 * GAMMA * GAMMA) - 0.5 * GAMMA);
  EXPECT_NEAR(result, expected, 1e-10);
}

TEST_F(CompactModelTest, SaturationVoltageDoubleSqrt) {
  // VDSS = Vc * (sqrt(0.25 + sqrt(if_) * Vt_Vc) - 0.5)
  // Saturation voltage with double square root (note: if is reserved, use if_)
  param("Vc", 0.8);
  param("if_", 100.0);  // Forward inversion coefficient
  param("Vt_Vc", 0.0325);  // Vt/Vc ratio

  double result = eval("Vc * (sqrt(0.25 + sqrt(if_) * Vt_Vc) - 0.5)");

  double Vc = 0.8, if_ = 100.0, Vt_Vc = 0.0325;
  double expected = Vc * (std::sqrt(0.25 + std::sqrt(if_) * Vt_Vc) - 0.5);
  EXPECT_NEAR(result, expected, 1e-10);
}

TEST_F(CompactModelTest, DepletionChargeExpLn) {
  // DFV_f = vd * (1.0 - exp(-ln(aj) / z))
  // HICUM-style depletion charge calculation
  param("vd", 0.8);
  param("aj", 2.5);
  param("z", 0.4);

  double result = eval("vd * (1.0 - exp(-ln(aj) / z))");

  double vd = 0.8, aj = 2.5, z = 0.4;
  double expected = vd * (1.0 - std::exp(-std::log(aj) / z));
  EXPECT_NEAR(result, expected, 1e-10);
}

TEST_F(CompactModelTest, JunctionCapacitancePower) {
  // Cj = Cj0 * exp(-mj * ln(1.0 + Vj / Pb))
  // Equivalent to Cj0 * (1 + Vj/Pb)^(-mj) for junction capacitance
  param("Cj0", 1e-12);
  param("mj", 0.5);
  param("Vj", 0.3);
  param("Pb", 0.8);

  double result = eval("Cj0 * exp(-mj * ln(1.0 + Vj / Pb))");

  double Cj0 = 1e-12, mj = 0.5, Vj = 0.3, Pb = 0.8;
  double expected = Cj0 * std::exp(-mj * std::log(1.0 + Vj / Pb));
  EXPECT_NEAR(result, expected, 1e-20);  // Small absolute value
}

TEST_F(CompactModelTest, ImpactIonizationCurrent) {
  // isub = IBA * Vib * exp(-Lc / Vib)
  // Substrate current due to impact ionization
  param("IBA", 1e-9);
  param("Vib", 2.5);
  param("Lc", 1.0);

  double result = eval("IBA * Vib * exp(-Lc / Vib)");

  double IBA = 1e-9, Vib = 2.5, Lc = 1.0;
  double expected = IBA * Vib * std::exp(-Lc / Vib);
  EXPECT_NEAR(result, expected, 1e-20);
}

TEST_F(CompactModelTest, ChannelLengthModulation) {
  // deltaL = Lc * ln(1.0 + (Vds - Vip) / Ucrit)
  // Channel length modulation with logarithm
  param("Lc", 0.1e-6);
  param("Vds", 1.8);
  param("Vip", 0.5);
  param("Ucrit", 5e6);

  double result = eval("Lc * ln(1.0 + (Vds - Vip) / Ucrit)");

  double Lc = 0.1e-6, Vds = 1.8, Vip = 0.5, Ucrit = 5e6;
  double expected = Lc * std::log(1.0 + (Vds - Vip) / Ucrit);
  EXPECT_NEAR(result, expected, 1e-20);
}

TEST_F(CompactModelTest, MobilityReductionEffectiveField) {
  // beta = KP * W / (L * (1.0 + T0 * abs(qb + eta * qi)))
  // Mobility reduction due to effective vertical field
  param("KP", 100e-6);
  param("W", 10e-6);
  param("L", 1e-6);
  param("T0", 0.5);
  param("qb", -0.3);
  param("eta", 0.5);
  param("qi", -0.8);

  double result = eval("KP * W / (L * (1.0 + T0 * abs(qb + eta * qi)))");

  double KP = 100e-6, W = 10e-6, L = 1e-6, T0 = 0.5, qb = -0.3, eta = 0.5, qi = -0.8;
  double expected = KP * W / (L * (1.0 + T0 * std::fabs(qb + eta * qi)));
  EXPECT_NEAR(result, expected, 1e-10);
}

// =============================================================================
// HICUM Model Style Tests
// =============================================================================

TEST_F(CompactModelTest, AvalancheCurrentComplex) {
  // av = favl * exp(-qavl / (vq0 * fc_av * Cjci))
  // avl = av * (vq0 + (1.0 + qavl/(vq0*Cjci)) * (Vci - vq0))
  // HICUM avalanche current with exponential damping
  param("favl", 1e-3);
  param("qavl", 1e-15);
  param("vq0", 0.5);
  param("fc_av", 0.9);
  param("Cjci", 1e-14);
  param("Vci", 3.0);

  // First calculate intermediate av
  double result_av = eval("favl * exp(-qavl / (vq0 * fc_av * Cjci))");

  double favl = 1e-3, qavl = 1e-15, vq0 = 0.5, fc_av = 0.9, Cjci = 1e-14;
  double expected_av = favl * std::exp(-qavl / (vq0 * fc_av * Cjci));
  EXPECT_NEAR(result_av, expected_av, 1e-10);

  // Now the full avalanche current expression
  param("av", expected_av);
  double result_avl = eval("av * (vq0 + (1.0 + qavl/(vq0*Cjci)) * (Vci - vq0))");

  double Vci = 3.0;
  double expected_avl = expected_av * (vq0 + (1.0 + qavl / (vq0 * Cjci)) * (Vci - vq0));
  EXPECT_NEAR(result_avl, expected_avl, 1e-10);
}

TEST_F(CompactModelTest, BaseResistanceModulation) {
  // fQz = 0.5 * (Qz_nom + sqrt(Qz_nom^2 + 0.01))
  // rbi = rbi0 / fQz
  // Base resistance conductivity modulation
  param("Qz_nom", 1.5);
  param("rbi0", 100.0);

  double result_fQz = eval("0.5 * (Qz_nom + sqrt(Qz_nom**2 + 0.01))");

  double Qz_nom = 1.5;
  double expected_fQz = 0.5 * (Qz_nom + std::sqrt(Qz_nom * Qz_nom + 0.01));
  EXPECT_NEAR(result_fQz, expected_fQz, 1e-10);

  param("fQz", expected_fQz);
  double result_rbi = eval("rbi0 / fQz");

  double rbi0 = 100.0;
  double expected_rbi = rbi0 / expected_fQz;
  EXPECT_NEAR(result_rbi, expected_rbi, 1e-10);
}

TEST_F(CompactModelTest, SaturationCurrentTemperatureScaling) {
  // ibeis_t = ibeis * exp(zetabet * ln(qtt0) + vge / Vt * (qtt0 - 1))
  // Temperature scaling of saturation current
  param("ibeis", 1e-18);
  param("zetabet", 3.5);
  param("qtt0", 1.1);  // T/Tnom ratio
  param("vge", 1.12);  // Bandgap voltage
  param("Vt", 0.0259);

  double result = eval("ibeis * exp(zetabet * ln(qtt0) + vge / Vt * (qtt0 - 1))");

  double ibeis = 1e-18, zetabet = 3.5, qtt0 = 1.1, vge = 1.12, Vt = 0.0259;
  double expected = ibeis * std::exp(zetabet * std::log(qtt0) + vge / Vt * (qtt0 - 1));
  EXPECT_NEAR(result, expected, 1e-20);
}

TEST_F(CompactModelTest, TransferCurrentQuadratic) {
  // Q_p = A + sqrt(A^2 + T_f0 * i_0f + tr * i_0r)
  // itf = i_0f / Q_p
  // Transfer current calculation with minority charge
  param("A", 0.5);
  param("T_f0", 1e-12);
  param("i_0f", 1e-3);
  param("tr", 1e-11);
  param("i_0r", 1e-6);

  double result_Qp = eval("A + sqrt(A**2 + T_f0 * i_0f + tr * i_0r)");

  double A = 0.5, T_f0 = 1e-12, i_0f = 1e-3, tr = 1e-11, i_0r = 1e-6;
  double expected_Qp = A + std::sqrt(A * A + T_f0 * i_0f + tr * i_0r);
  EXPECT_NEAR(result_Qp, expected_Qp, 1e-10);

  param("Q_p", expected_Qp);
  double result_itf = eval("i_0f / Q_p");

  double expected_itf = i_0f / expected_Qp;
  EXPECT_NEAR(result_itf, expected_itf, 1e-10);
}

TEST_F(CompactModelTest, DrainChargePolynomial) {
  // QD = -n * Vt * COX * (0.267 * (3*s^3 + 6*s^2*f + 4*s*f^2 + 2*f^3) / (s+f)^2 - 0.5)
  // Drain charge calculation with polynomial terms
  param("n", 1.3);
  param("Vt", 0.0259);
  param("COX", 1e-14);
  param("s", 0.8);  // Source inversion coefficient
  param("f", 0.3);  // Drain inversion coefficient

  double result = eval("-n * Vt * COX * (0.267 * (3*s**3 + 6*s**2*f + 4*s*f**2 + 2*f**3) / (s+f)**2 - 0.5)");

  double n = 1.3, Vt = 0.0259, COX = 1e-14, s = 0.8, f = 0.3;
  double poly = 3 * s * s * s + 6 * s * s * f + 4 * s * f * f + 2 * f * f * f;
  double denom = (s + f) * (s + f);
  double expected = -n * Vt * COX * (0.267 * poly / denom - 0.5);
  EXPECT_NEAR(result, expected, 1e-25);
}

TEST_F(CompactModelTest, SmoothingFunctionTernary) {
  // result = (x > 0) ? sqrt(x*x + smooth) : -sqrt(x*x + smooth)
  // Smooth sign function using ternary
  param("smooth", 1e-6);

  // Test positive x
  param("x", 2.5);
  double result_pos = eval("(x > 0) ? sqrt(x*x + smooth) : -sqrt(x*x + smooth)");
  double expected_pos = std::sqrt(2.5 * 2.5 + 1e-6);
  EXPECT_NEAR(result_pos, expected_pos, 1e-10);

  // Test negative x (redefine)
  engine.set_parameter_override("x", engine.arena().make<NumberLiteral>(-2.5));
  double result_neg = eval("(x > 0) ? sqrt(x*x + smooth) : -sqrt(x*x + smooth)");
  double expected_neg = -std::sqrt(2.5 * 2.5 + 1e-6);
  EXPECT_NEAR(result_neg, expected_neg, 1e-10);
}

// =============================================================================
// Combined Complex Expressions
// =============================================================================

TEST_F(CompactModelTest, MOSFETDrainCurrentSaturation) {
  // ids = (kp/2) * (W/L) * (Vgs - Vth)^2 * (1 + lambda * Vds)
  // Standard MOSFET saturation current with channel length modulation
  param("kp", 100e-6);
  param("W", 10e-6);
  param("L", 0.18e-6);
  param("Vgs", 1.2);
  param("Vth", 0.4);
  param("lambda", 0.05);
  param("Vds", 1.8);

  double result = eval("(kp/2) * (W/L) * (Vgs - Vth)**2 * (1 + lambda * Vds)");

  double kp = 100e-6, W = 10e-6, L = 0.18e-6, Vgs = 1.2, Vth = 0.4, lambda = 0.05, Vds = 1.8;
  double expected = (kp / 2) * (W / L) * std::pow(Vgs - Vth, 2) * (1 + lambda * Vds);
  EXPECT_NEAR(result, expected, 1e-10);
}

TEST_F(CompactModelTest, BJTCollectorCurrentGummelPoon) {
  // Ic = Is * exp(Vbe/Vt) * (1 - exp(-Vce/Vt)) / (1 + Vbe/(VA + Vce))
  // Simplified Gummel-Poon BJT collector current
  param("Is", 1e-15);
  param("Vbe", 0.7);
  param("Vce", 2.0);
  param("Vt", 0.0259);
  param("VA", 100.0);

  double result = eval("Is * exp(Vbe/Vt) * (1 - exp(-Vce/Vt)) / (1 + Vbe/(VA + Vce))");

  double Is = 1e-15, Vbe = 0.7, Vce = 2.0, Vt = 0.0259, VA = 100.0;
  double expected = Is * std::exp(Vbe / Vt) * (1 - std::exp(-Vce / Vt)) / (1 + Vbe / (VA + Vce));
  EXPECT_NEAR(result, expected, 1e-10);
}

TEST_F(CompactModelTest, DiodeCurrentWithSeriesResistance) {
  // Iterative-like expression: I = Is * (exp((V - I*Rs)/Vt) - 1)
  // Using smoothed version: I_approx = Is * exp(V/Vt) / (1 + Is*Rs*exp(V/Vt)/Vt)
  param("Is", 1e-14);
  param("V", 0.6);
  param("Rs", 10.0);
  param("Vt", 0.0259);

  double result = eval("Is * exp(V/Vt) / (1 + Is*Rs*exp(V/Vt)/Vt)");

  double Is = 1e-14, V = 0.6, Rs = 10.0, Vt = 0.0259;
  double exp_term = std::exp(V / Vt);
  double expected = Is * exp_term / (1 + Is * Rs * exp_term / Vt);
  EXPECT_NEAR(result, expected, 1e-15);
}

TEST_F(CompactModelTest, SubthresholdSwingExpression) {
  // Ids_sub = I0 * exp((Vgs - Vth) / (n * Vt)) * (1 - exp(-Vds/Vt))
  // Subthreshold current with DIBL effect
  param("I0", 1e-9);
  param("Vgs", 0.3);
  param("Vth", 0.4);
  param("n", 1.4);
  param("Vt", 0.0259);
  param("Vds", 0.1);

  double result = eval("I0 * exp((Vgs - Vth) / (n * Vt)) * (1 - exp(-Vds/Vt))");

  double I0 = 1e-9, Vgs = 0.3, Vth = 0.4, n = 1.4, Vt = 0.0259, Vds = 0.1;
  double expected = I0 * std::exp((Vgs - Vth) / (n * Vt)) * (1 - std::exp(-Vds / Vt));
  EXPECT_NEAR(result, expected, 1e-15);
}

TEST_F(CompactModelTest, ThermalVoltageTemperatureDependence) {
  // Vt = k * T / q = 8.617e-5 * T (in eV, using k/q directly)
  // Eg(T) = Eg0 - alpha * T^2 / (T + beta)
  // ni(T) = ni0 * (T/T0)^1.5 * exp(-Eg(T)/(2*Vt) + Eg0/(2*Vt0))
  param("T", 350.0);
  param("T0", 300.0);
  param("Eg0", 1.12);
  param("alpha", 4.73e-4);
  param("beta", 636.0);
  param("ni0", 1e10);
  param("Vt0", 0.0259);

  // First calculate Vt at T
  double result_Vt = eval("8.617e-5 * T");
  double T = 350.0;
  double expected_Vt = 8.617e-5 * T;
  EXPECT_NEAR(result_Vt, expected_Vt, 1e-10);

  // Calculate Eg(T)
  param("Vt", expected_Vt);
  double result_Eg = eval("Eg0 - alpha * T**2 / (T + beta)");
  double Eg0 = 1.12, alpha = 4.73e-4, beta = 636.0;
  double expected_Eg = Eg0 - alpha * T * T / (T + beta);
  EXPECT_NEAR(result_Eg, expected_Eg, 1e-10);

  // Calculate ni(T)
  param("Eg", expected_Eg);
  double result_ni = eval("ni0 * pow(T/T0, 1.5) * exp(-Eg/(2*Vt) + Eg0/(2*Vt0))");
  double T0 = 300.0, ni0 = 1e10, Vt0 = 0.0259;
  double expected_ni = ni0 * std::pow(T / T0, 1.5) * std::exp(-expected_Eg / (2 * expected_Vt) + Eg0 / (2 * Vt0));
  EXPECT_NEAR(result_ni, expected_ni, 1e-2);  // Larger tolerance due to accumulated precision
}

// =============================================================================
// Parameter Dependency Chain Tests
// =============================================================================
// These tests verify that the expression engine correctly resolves multi-level
// parameter dependencies where param A depends on B, B depends on C, etc.

TEST_F(CompactModelTest, DependencyChain_TemperatureModel) {
  // Realistic temperature model dependency chain:
  // T (base) -> Vt -> Eg -> ni
  // Each parameter is defined as an expression, not a constant
  ExprArena& arena = engine.arena();

  // Level 0: Base temperature (constant)
  engine.define_parameter("T", arena.make<NumberLiteral>(350.0));
  engine.define_parameter("T0", arena.make<NumberLiteral>(300.0));
  engine.define_parameter("Eg0", arena.make<NumberLiteral>(1.12));
  engine.define_parameter("ni0", arena.make<NumberLiteral>(1e10));
  engine.define_parameter("Vt0", arena.make<NumberLiteral>(0.0259));
  engine.define_parameter("alpha", arena.make<NumberLiteral>(4.73e-4));
  engine.define_parameter("beta", arena.make<NumberLiteral>(636.0));

  // Level 1: Vt depends on T
  // Vt = 8.617e-5 * T
  engine.define_parameter("Vt", parse_expression("8.617e-5 * T", arena));

  // Level 2: Eg depends on T
  // Eg = Eg0 - alpha * T^2 / (T + beta)
  engine.define_parameter("Eg", parse_expression("Eg0 - alpha * T**2 / (T + beta)", arena));

  // Level 3: ni depends on Vt, Eg, T (multi-dependency)
  // ni = ni0 * (T/T0)^1.5 * exp(-Eg/(2*Vt) + Eg0/(2*Vt0))
  engine.define_parameter("ni", parse_expression("ni0 * pow(T/T0, 1.5) * exp(-Eg/(2*Vt) + Eg0/(2*Vt0))", arena));

  // Build evaluation order - this resolves the dependency chain
  engine.build_evaluation_order();
  EXPECT_TRUE(engine.has_valid_evaluation_order());
  EXPECT_FALSE(engine.has_cyclic_dependencies());

  // Evaluate ni - should automatically resolve Vt, Eg first
  double result_ni = engine.evaluate_real(*arena.make<Identifier>("ni"));

  // Calculate expected values manually
  double T = 350.0, T0 = 300.0, Eg0 = 1.12, ni0 = 1e10, Vt0 = 0.0259;
  double alpha = 4.73e-4, beta = 636.0;
  double expected_Vt = 8.617e-5 * T;
  double expected_Eg = Eg0 - alpha * T * T / (T + beta);
  double expected_ni = ni0 * std::pow(T / T0, 1.5) *
                       std::exp(-expected_Eg / (2 * expected_Vt) + Eg0 / (2 * Vt0));

  EXPECT_NEAR(result_ni, expected_ni, 1e-2);

  // Also verify intermediate parameters are correctly resolved
  double result_Vt = engine.evaluate_real(*arena.make<Identifier>("Vt"));
  EXPECT_NEAR(result_Vt, expected_Vt, 1e-10);

  double result_Eg = engine.evaluate_real(*arena.make<Identifier>("Eg"));
  EXPECT_NEAR(result_Eg, expected_Eg, 1e-10);
}

TEST_F(CompactModelTest, DependencyChain_MOSFETModel) {
  // MOSFET model with derived parameters:
  // W, L (base) -> WL_ratio -> beta -> Ids
  ExprArena& arena = engine.arena();

  // Base parameters
  engine.define_parameter("W", arena.make<NumberLiteral>(10e-6));
  engine.define_parameter("L", arena.make<NumberLiteral>(0.18e-6));
  engine.define_parameter("KP", arena.make<NumberLiteral>(100e-6));
  engine.define_parameter("Vgs", arena.make<NumberLiteral>(1.2));
  engine.define_parameter("Vth0", arena.make<NumberLiteral>(0.35));
  engine.define_parameter("lambda", arena.make<NumberLiteral>(0.05));
  engine.define_parameter("Vds", arena.make<NumberLiteral>(1.8));
  engine.define_parameter("eta", arena.make<NumberLiteral>(0.04));

  // Level 1: W/L ratio
  engine.define_parameter("WL_ratio", parse_expression("W / L", arena));

  // Level 2: Vth with DIBL effect (depends on Vds)
  // Vth = Vth0 - eta * Vds
  engine.define_parameter("Vth", parse_expression("Vth0 - eta * Vds", arena));

  // Level 3: Effective transconductance parameter
  // beta = KP * WL_ratio
  engine.define_parameter("beta", parse_expression("KP * WL_ratio", arena));

  // Level 4: Overdrive voltage (depends on Vth)
  // Vov = Vgs - Vth
  engine.define_parameter("Vov", parse_expression("Vgs - Vth", arena));

  // Level 5: Drain current (depends on beta, Vov, lambda, Vds)
  // Ids = (beta/2) * Vov^2 * (1 + lambda * Vds)
  engine.define_parameter("Ids", parse_expression("(beta/2) * Vov**2 * (1 + lambda * Vds)", arena));

  // Build and verify
  engine.build_evaluation_order();
  EXPECT_TRUE(engine.has_valid_evaluation_order());
  EXPECT_FALSE(engine.has_cyclic_dependencies());

  // Evaluate Ids
  double result_Ids = engine.evaluate_real(*arena.make<Identifier>("Ids"));

  // Manual calculation
  double W = 10e-6, L = 0.18e-6, KP = 100e-6, Vgs = 1.2, Vth0 = 0.35;
  double lambda = 0.05, Vds = 1.8, eta = 0.04;
  double WL_ratio = W / L;
  double Vth = Vth0 - eta * Vds;
  double beta = KP * WL_ratio;
  double Vov = Vgs - Vth;
  double expected_Ids = (beta / 2) * Vov * Vov * (1 + lambda * Vds);

  EXPECT_NEAR(result_Ids, expected_Ids, 1e-10);

  // Verify intermediate values
  EXPECT_NEAR(engine.evaluate_real(*arena.make<Identifier>("WL_ratio")), WL_ratio, 1e-10);
  EXPECT_NEAR(engine.evaluate_real(*arena.make<Identifier>("Vth")), Vth, 1e-10);
  EXPECT_NEAR(engine.evaluate_real(*arena.make<Identifier>("beta")), beta, 1e-10);
  EXPECT_NEAR(engine.evaluate_real(*arena.make<Identifier>("Vov")), Vov, 1e-10);
}

TEST_F(CompactModelTest, DependencyChain_BJTEarlyEffect) {
  // BJT model with Early effect:
  // Is, VA (base) -> ro -> Ic_corrected
  ExprArena& arena = engine.arena();

  // Base parameters
  engine.define_parameter("Is", arena.make<NumberLiteral>(1e-15));
  engine.define_parameter("Vbe", arena.make<NumberLiteral>(0.7));
  engine.define_parameter("Vce", arena.make<NumberLiteral>(5.0));
  engine.define_parameter("VA", arena.make<NumberLiteral>(100.0));
  engine.define_parameter("Vt", arena.make<NumberLiteral>(0.0259));
  engine.define_parameter("BF", arena.make<NumberLiteral>(100.0));

  // Level 1: Base collector current (ideal)
  // Ic0 = Is * exp(Vbe/Vt)
  engine.define_parameter("Ic0", parse_expression("Is * exp(Vbe/Vt)", arena));

  // Level 2: Early effect factor
  // early_factor = 1 + Vce/VA
  engine.define_parameter("early_factor", parse_expression("1 + Vce/VA", arena));

  // Level 3: Collector current with Early effect
  // Ic = Ic0 * early_factor
  engine.define_parameter("Ic", parse_expression("Ic0 * early_factor", arena));

  // Level 4: Base current (depends on Ic)
  // Ib = Ic / BF
  engine.define_parameter("Ib", parse_expression("Ic / BF", arena));

  // Level 5: Output conductance (depends on Ic0)
  // gce = Ic0 / VA
  engine.define_parameter("gce", parse_expression("Ic0 / VA", arena));

  engine.build_evaluation_order();
  EXPECT_TRUE(engine.has_valid_evaluation_order());

  // Manual calculation
  double Is = 1e-15, Vbe = 0.7, Vce = 5.0, VA = 100.0, Vt = 0.0259, BF = 100.0;
  double Ic0 = Is * std::exp(Vbe / Vt);
  double early_factor = 1 + Vce / VA;
  double Ic = Ic0 * early_factor;
  double Ib = Ic / BF;
  double gce = Ic0 / VA;

  EXPECT_NEAR(engine.evaluate_real(*arena.make<Identifier>("Ic")), Ic, 1e-10);
  EXPECT_NEAR(engine.evaluate_real(*arena.make<Identifier>("Ib")), Ib, 1e-15);
  EXPECT_NEAR(engine.evaluate_real(*arena.make<Identifier>("gce")), gce, 1e-20);
}

TEST_F(CompactModelTest, DependencyChain_DiamondPattern) {
  // Diamond dependency pattern:
  //       A
  //      / \
  //     B   C
  //      \ /
  //       D
  // D depends on both B and C, which both depend on A
  ExprArena& arena = engine.arena();

  // Base: A
  engine.define_parameter("A", arena.make<NumberLiteral>(10.0));

  // Level 1: B and C both depend on A
  engine.define_parameter("B", parse_expression("A * 2", arena));
  engine.define_parameter("C", parse_expression("A + 5", arena));

  // Level 2: D depends on both B and C
  engine.define_parameter("D", parse_expression("B * C", arena));

  engine.build_evaluation_order();
  EXPECT_TRUE(engine.has_valid_evaluation_order());

  double A = 10.0;
  double B = A * 2;      // 20
  double C = A + 5;      // 15
  double D = B * C;      // 300

  EXPECT_NEAR(engine.evaluate_real(*arena.make<Identifier>("D")), D, 1e-10);
}

TEST_F(CompactModelTest, DependencyChain_DeepNesting) {
  // Deep 6-level dependency chain:
  // L0 -> L1 -> L2 -> L3 -> L4 -> L5
  ExprArena& arena = engine.arena();

  engine.define_parameter("L0", arena.make<NumberLiteral>(1.0));
  engine.define_parameter("L1", parse_expression("L0 + 1", arena));
  engine.define_parameter("L2", parse_expression("L1 * 2", arena));
  engine.define_parameter("L3", parse_expression("sqrt(L2 + 10)", arena));
  engine.define_parameter("L4", parse_expression("exp(L3 / 10)", arena));
  engine.define_parameter("L5", parse_expression("ln(L4) * L0 + L2", arena));

  engine.build_evaluation_order();
  EXPECT_TRUE(engine.has_valid_evaluation_order());

  // Manual
  double L0 = 1.0;
  double L1 = L0 + 1;           // 2
  double L2 = L1 * 2;           // 4
  double L3 = std::sqrt(L2 + 10);  // sqrt(14)
  double L4 = std::exp(L3 / 10);
  double L5 = std::log(L4) * L0 + L2;

  EXPECT_NEAR(engine.evaluate_real(*arena.make<Identifier>("L5")), L5, 1e-10);
}

TEST_F(CompactModelTest, DependencyChain_EKVStyleFullModel) {
  // Full EKV-style model with realistic parameter dependencies
  ExprArena& arena = engine.arena();

  // Technology parameters (base)
  engine.define_parameter("T", arena.make<NumberLiteral>(300.0));
  engine.define_parameter("COX", arena.make<NumberLiteral>(8.5e-3));  // F/m^2
  engine.define_parameter("PHI0", arena.make<NumberLiteral>(0.7));
  engine.define_parameter("GAMMA0", arena.make<NumberLiteral>(0.5));
  engine.define_parameter("VTO", arena.make<NumberLiteral>(0.4));
  engine.define_parameter("KP0", arena.make<NumberLiteral>(100e-6));

  // Geometry
  engine.define_parameter("W", arena.make<NumberLiteral>(10e-6));
  engine.define_parameter("L", arena.make<NumberLiteral>(0.5e-6));

  // Bias
  engine.define_parameter("VG", arena.make<NumberLiteral>(1.2));
  engine.define_parameter("VS", arena.make<NumberLiteral>(0.0));
  engine.define_parameter("VD", arena.make<NumberLiteral>(1.8));
  engine.define_parameter("VB", arena.make<NumberLiteral>(0.0));

  // Level 1: Thermal voltage
  engine.define_parameter("Vt", parse_expression("8.617e-5 * T", arena));

  // Level 2: Effective gate voltage
  engine.define_parameter("VGprime", parse_expression("VG - VTO + PHI0", arena));

  // Level 3: Body effect parameter (simplified)
  engine.define_parameter("GAMMA", parse_expression("GAMMA0 * sqrt(1 + VS/PHI0)", arena));

  // Level 4: Pinch-off voltage
  engine.define_parameter("VP", parse_expression(
      "VGprime - GAMMA * (sqrt(VGprime + 0.25*GAMMA**2) - 0.5*GAMMA)", arena));

  // Level 5: Slope factor
  engine.define_parameter("n", parse_expression("1 + GAMMA / (2 * sqrt(PHI0 + VP))", arena));

  // Level 6: Transconductance
  engine.define_parameter("beta", parse_expression("KP0 * W / L", arena));

  // Level 7: Forward normalized current
  engine.define_parameter("if_", parse_expression(
      "pow(ln(1 + exp((VP - VS) / (2 * n * Vt))), 2)", arena));

  // Level 8: Reverse normalized current
  engine.define_parameter("ir", parse_expression(
      "pow(ln(1 + exp((VP - VD) / (2 * n * Vt))), 2)", arena));

  // Level 9: Drain current (simplified EKV form)
  engine.define_parameter("Ids", parse_expression("beta * n * Vt**2 * (if_ - ir)", arena));

  engine.build_evaluation_order();
  EXPECT_TRUE(engine.has_valid_evaluation_order());
  EXPECT_FALSE(engine.has_cyclic_dependencies());

  // Evaluate the final drain current
  double result_Ids = engine.evaluate_real(*arena.make<Identifier>("Ids"));

  // Manual calculation for verification
  double T = 300.0, PHI0 = 0.7, GAMMA0 = 0.5, VTO = 0.4, KP0 = 100e-6;
  double W = 10e-6, L = 0.5e-6;
  double VG = 1.2, VS = 0.0, VD = 1.8;

  double Vt = 8.617e-5 * T;
  double VGprime = VG - VTO + PHI0;
  double GAMMA = GAMMA0 * std::sqrt(1 + VS / PHI0);
  double VP = VGprime - GAMMA * (std::sqrt(VGprime + 0.25 * GAMMA * GAMMA) - 0.5 * GAMMA);
  double n = 1 + GAMMA / (2 * std::sqrt(PHI0 + VP));
  double beta = KP0 * W / L;
  double if_ = std::pow(std::log(1 + std::exp((VP - VS) / (2 * n * Vt))), 2);
  double ir = std::pow(std::log(1 + std::exp((VP - VD) / (2 * n * Vt))), 2);
  double expected_Ids = beta * n * Vt * Vt * (if_ - ir);

  EXPECT_NEAR(result_Ids, expected_Ids, 1e-10);

  // Verify the device is in saturation (ir << if_)
  EXPECT_GT(engine.evaluate_real(*arena.make<Identifier>("if_")),
            engine.evaluate_real(*arena.make<Identifier>("ir")) * 100);
}
