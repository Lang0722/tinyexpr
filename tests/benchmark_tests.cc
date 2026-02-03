#include <benchmark/benchmark.h>

#include <cmath>
#include <string>
#include <vector>

#include "spice_expr/spice_expr.h"

using namespace spice_expr;

// Benchmark expressions adapted from ExprTK (exprtk_benchmark.cpp)
// Syntax adaptations: x^y -> x**y, clamp(l,v,u) -> limit(v,l,u), if(c,t,f) -> c ? t : f
// pi -> 3.14159265358979323846 (literal value)
static const std::vector<std::string> benchmark_expressions = {
    "(y + x)",
    "2 * (y + x)",
    "(2 * y + 2 * x)",
    "((1.23 * x**2) / y) - 123.123",
    "(y + x / y) * (x - y / x)",
    "x / ((x + y) + (x - y)) / y",
    "1 - ((x * y) + (y / x)) - 3",
    "(5.5 + x) + (2 * x - 2 / 3 * y) * (x / 3 + y / 4) + (y + 7.7)",
    "1.1*x**1 + 2.2*y**2 - 3.3*x**3 + 4.4*y**15 - 5.5*x**23 + 6.6*y**55",
    "sin(2 * x) + cos(3.14159265358979323846 / y)",
    "1 - sin(2 * x) + cos(3.14159265358979323846 / y)",
    "sqrt(111.111 - sin(2 * x) + cos(3.14159265358979323846 / y) / 333.333)",
    "(x**2 / sin(2 * 3.14159265358979323846 / y)) - x / 2",
    "x + (cos(y - sin(2 / x * 3.14159265358979323846)) - sin(x - cos(2 * y / "
    "3.14159265358979323846))) - y",
    "limit(sin(2 * 3.14159265358979323846 * x) + cos(y / 2 * 3.14159265358979323846), -1.0, "
    "+1.0)",
    "max(3.33, min(sqrt(1 - sin(2 * x) + cos(3.14159265358979323846 / y) / 3), 1.11))",
    "((y + (x * 2.2)) <= (x + y + 1.1)) ? (x - y) : (x * y)",
};

// Native C++ implementations for comparison
namespace native {
const double pi = 3.14159265358979323846;

inline double func00(double x, double y) { return (y + x); }
inline double func01(double x, double y) { return 2.0 * (y + x); }
inline double func02(double x, double y) { return (2.0 * y + 2.0 * x); }
inline double func03(double x, double y) { return ((1.23 * x * x) / y) - 123.123; }
inline double func04(double x, double y) { return (y + x / y) * (x - y / x); }
inline double func05(double x, double y) { return x / ((x + y) + (x - y)) / y; }
inline double func06(double x, double y) { return 1.0 - ((x * y) + (y / x)) - 3.0; }
inline double func07(double x, double y) {
  return (5.5 + x) + (2.0 * x - 2.0 / 3.0 * y) * (x / 3.0 + y / 4.0) + (y + 7.7);
}
inline double func08(double x, double y) {
  return (1.1 * std::pow(x, 1.0) + 2.2 * std::pow(y, 2.0) - 3.3 * std::pow(x, 3.0) +
          4.4 * std::pow(y, 15.0) - 5.5 * std::pow(x, 23.0) + 6.6 * std::pow(y, 55.0));
}
inline double func09(double x, double y) { return std::sin(2.0 * x) + std::cos(pi / y); }
inline double func10(double x, double y) { return 1.0 - std::sin(2.0 * x) + std::cos(pi / y); }
inline double func11(double x, double y) {
  return std::sqrt(111.111 - std::sin(2.0 * x) + std::cos(pi / y) / 333.333);
}
inline double func12(double x, double y) {
  return ((x * x) / std::sin(2.0 * pi / y)) - x / 2.0;
}
inline double func13(double x, double y) {
  return (x + (std::cos(y - std::sin(2.0 / x * pi)) - std::sin(x - std::cos(2.0 * y / pi))) - y);
}
inline double func14(double x, double y) {
  double val = std::sin(2.0 * pi * x) + std::cos(y / 2.0 * pi);
  return (val < -1.0) ? -1.0 : ((val > 1.0) ? 1.0 : val);
}
inline double func15(double x, double y) {
  return std::max(3.33, std::min(std::sqrt(1.0 - std::sin(2.0 * x) + std::cos(pi / y) / 3.0), 1.11));
}
inline double func16(double x, double y) {
  return (((y + (x * 2.2)) <= (x + y + 1.1)) ? (x - y) : (x * y));
}

typedef double (*NativeFunc)(double, double);
static const NativeFunc native_funcs[] = {func00, func01, func02, func03, func04, func05,
                                          func06, func07, func08, func09, func10, func11,
                                          func12, func13, func14, func15, func16};
}  // namespace native

// Parsing benchmark - measures parse throughput
static void BM_Parse(benchmark::State& state) {
  const size_t expr_idx = state.range(0);
  const std::string& expr_str = benchmark_expressions[expr_idx];

  for (auto _ : state) {
    ExprArena arena;
    ExprNode* expr = parse_expression(expr_str, arena);
    benchmark::DoNotOptimize(expr);
  }

  state.SetLabel(expr_str.substr(0, 40));
}

// Evaluation benchmark - measures evaluation throughput
static void BM_Evaluate(benchmark::State& state) {
  const size_t expr_idx = state.range(0);
  const std::string& expr_str = benchmark_expressions[expr_idx];

  ExprArena arena;
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;

  auto* x_expr = arena.make<NumberLiteral>(2.5);
  auto* y_expr = arena.make<NumberLiteral>(3.5);
  symbols.define("x", x_expr);
  symbols.define("y", y_expr);

  ExprNode* expr = parse_expression(expr_str, arena);
  RealEvaluator eval(symbols, functions, circuit);

  for (auto _ : state) {
    double result = eval.evaluate(*expr);
    benchmark::DoNotOptimize(result);
  }

  state.SetLabel(expr_str.substr(0, 40));
}

// Native C++ evaluation benchmark for comparison
static void BM_Native(benchmark::State& state) {
  const size_t expr_idx = state.range(0);
  double x = 2.5;
  double y = 3.5;
  auto func = native::native_funcs[expr_idx];

  for (auto _ : state) {
    double result = func(x, y);
    benchmark::DoNotOptimize(result);
  }

  state.SetLabel(benchmark_expressions[expr_idx].substr(0, 40));
}

// Variable iteration benchmark - simulates sweeping parameters
static void BM_EvaluateIteration(benchmark::State& state) {
  const size_t expr_idx = state.range(0);
  const std::string& expr_str = benchmark_expressions[expr_idx];

  ExprArena arena;
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;

  auto* x_expr = arena.make<NumberLiteral>(0.0);
  auto* y_expr = arena.make<NumberLiteral>(0.0);
  symbols.define("x", x_expr);
  symbols.define("y", y_expr);

  ExprNode* expr = parse_expression(expr_str, arena);
  RealEvaluator eval(symbols, functions, circuit);

  for (auto _ : state) {
    double total = 0.0;
    for (double x = -5.0; x <= 5.0; x += 1.0) {
      for (double y = -5.0; y <= 5.0; y += 1.0) {
        symbols.set_override("x", arena.make<NumberLiteral>(x));
        symbols.set_override("y", arena.make<NumberLiteral>(y));
        total += eval.evaluate(*expr);
      }
    }
    benchmark::DoNotOptimize(total);
    symbols.clear_override("x");
    symbols.clear_override("y");
  }

  state.SetLabel(expr_str.substr(0, 40));
}

// Register benchmarks for each expression
BENCHMARK(BM_Parse)->DenseRange(0, benchmark_expressions.size() - 1);
BENCHMARK(BM_Evaluate)->DenseRange(0, benchmark_expressions.size() - 1);
BENCHMARK(BM_Native)->DenseRange(0, benchmark_expressions.size() - 1);
BENCHMARK(BM_EvaluateIteration)->DenseRange(0, 4);  // Run iteration only for simple expressions

// Additional benchmarks for specific scenarios

// Deep nesting benchmark
static void BM_DeepNesting(benchmark::State& state) {
  const int depth = state.range(0);
  std::string expr = "x";
  for (int i = 0; i < depth; ++i) {
    expr = "sin(" + expr + ")";
  }

  ExprArena arena;
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;

  symbols.define("x", arena.make<NumberLiteral>(0.5));
  ExprNode* parsed = parse_expression(expr, arena);
  RealEvaluator eval(symbols, functions, circuit);

  for (auto _ : state) {
    double result = eval.evaluate(*parsed);
    benchmark::DoNotOptimize(result);
  }
}
BENCHMARK(BM_DeepNesting)->Range(1, 32);

// Large expression benchmark (sum of many terms)
static void BM_LargeExpression(benchmark::State& state) {
  const int terms = state.range(0);
  std::string expr;
  for (int i = 0; i < terms; ++i) {
    if (i > 0) expr += " + ";
    expr += std::to_string(i + 1) + ".0 * x";
  }

  ExprArena arena;
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;

  symbols.define("x", arena.make<NumberLiteral>(1.0));
  ExprNode* parsed = parse_expression(expr, arena);
  RealEvaluator eval(symbols, functions, circuit);

  for (auto _ : state) {
    double result = eval.evaluate(*parsed);
    benchmark::DoNotOptimize(result);
  }
}
BENCHMARK(BM_LargeExpression)->Range(10, 1000);

// Parameter resolution benchmark
static void BM_ParameterChain(benchmark::State& state) {
  const int chain_length = state.range(0);

  ExprArena arena;
  SymbolTable symbols;
  FunctionRegistry functions;
  NullCircuitInterface circuit;

  // Create chain: p0 = 1, p1 = p0 + 1, p2 = p1 + 1, ...
  symbols.define("p0", arena.make<NumberLiteral>(1.0));
  for (int i = 1; i < chain_length; ++i) {
    auto* prev = arena.make<Identifier>("p" + std::to_string(i - 1));
    auto* one = arena.make<NumberLiteral>(1.0);
    auto* sum = arena.make<BinaryOp>(BinaryOpType::Add, prev, one);
    symbols.define("p" + std::to_string(i), sum);
  }

  ExprNode* final_param = arena.make<Identifier>("p" + std::to_string(chain_length - 1));
  RealEvaluator eval(symbols, functions, circuit);

  for (auto _ : state) {
    double result = eval.evaluate(*final_param);
    benchmark::DoNotOptimize(result);
  }
}
BENCHMARK(BM_ParameterChain)->Range(1, 100);
