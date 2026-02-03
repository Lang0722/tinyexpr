#include "spice_expr/function/function_registry.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <stdexcept>

namespace spice_expr {

namespace {

constexpr double PI = 3.14159265358979323846;

double safe_log(double x) {
  if (x <= 0)
    throw std::runtime_error("log of non-positive number");
  return std::log(x);
}

double safe_log10(double x) {
  if (x <= 0)
    throw std::runtime_error("log10 of non-positive number");
  return std::log10(x);
}

double safe_sqrt(double x) {
  if (x < 0)
    throw std::runtime_error("sqrt of negative number");
  return std::sqrt(x);
}

double db(double x) {
  if (x <= 0)
    throw std::runtime_error("db of non-positive number");
  return 20.0 * std::log10(x);
}

double pwr(double x, double y) {
  return std::pow(std::abs(x), y);
}

double pwrs(double x, double y) {
  double result = std::pow(std::abs(x), y);
  return (x < 0) ? -result : result;
}

double limit(double x, double low, double high) {
  return std::max(low, std::min(x, high));
}

double hypot_func(double x, double y) {
  return std::hypot(x, y);
}

double atan2_func(double y, double x) {
  return std::atan2(y, x);
}

double minN(const std::vector<double>& args) {
  if (args.empty())
    throw std::runtime_error("min requires at least one argument");
  return *std::min_element(args.begin(), args.end());
}

double maxN(const std::vector<double>& args) {
  if (args.empty())
    throw std::runtime_error("max requires at least one argument");
  return *std::max_element(args.begin(), args.end());
}

std::complex<double> clog(std::complex<double> x) {
  return std::log(x);
}

std::complex<double> clog10(std::complex<double> x) {
  return std::log10(x);
}

std::complex<double> csqrt(std::complex<double> x) {
  return std::sqrt(x);
}

}  // namespace

FunctionRegistry::FunctionRegistry() {
  register_builtins();
}

void FunctionRegistry::register_builtins() {
  // Trigonometric functions
  register_builtin(
      "sin", 1, 1, [](double x) { return std::sin(x); },
      [](std::complex<double> x) { return std::sin(x); });
  register_builtin(
      "cos", 1, 1, [](double x) { return std::cos(x); },
      [](std::complex<double> x) { return std::cos(x); });
  register_builtin(
      "tan", 1, 1, [](double x) { return std::tan(x); },
      [](std::complex<double> x) { return std::tan(x); });
  register_builtin(
      "asin", 1, 1, [](double x) { return std::asin(x); },
      [](std::complex<double> x) { return std::asin(x); });
  register_builtin(
      "acos", 1, 1, [](double x) { return std::acos(x); },
      [](std::complex<double> x) { return std::acos(x); });
  register_builtin(
      "atan", 1, 1, [](double x) { return std::atan(x); },
      [](std::complex<double> x) { return std::atan(x); });

  // Hyperbolic functions
  register_builtin(
      "sinh", 1, 1, [](double x) { return std::sinh(x); },
      [](std::complex<double> x) { return std::sinh(x); });
  register_builtin(
      "cosh", 1, 1, [](double x) { return std::cosh(x); },
      [](std::complex<double> x) { return std::cosh(x); });
  register_builtin(
      "tanh", 1, 1, [](double x) { return std::tanh(x); },
      [](std::complex<double> x) { return std::tanh(x); });

  // Exponential and logarithmic
  register_builtin(
      "exp", 1, 1, [](double x) { return std::exp(x); },
      [](std::complex<double> x) { return std::exp(x); });
  register_builtin("log", 1, 1, safe_log, clog);
  register_builtin("ln", 1, 1, safe_log, clog);
  register_builtin("log10", 1, 1, safe_log10, clog10);
  register_builtin("sqrt", 1, 1, safe_sqrt, csqrt);

  // Absolute value and rounding
  register_builtin(
      "abs", 1, 1, [](double x) { return std::fabs(x); },
      [](std::complex<double> x) { return std::complex<double>(std::abs(x), 0); });
  register_builtin("floor", 1, 1, [](double x) { return std::floor(x); }, nullptr);
  register_builtin("ceil", 1, 1, [](double x) { return std::ceil(x); }, nullptr);
  register_builtin("round", 1, 1, [](double x) { return std::round(x); }, nullptr);
  register_builtin("int", 1, 1, [](double x) { return std::trunc(x); }, nullptr);
  register_builtin(
      "sgn", 1, 1, [](double x) { return (x > 0) ? 1.0 : ((x < 0) ? -1.0 : 0.0); }, nullptr);
  register_builtin("db", 1, 1, db, nullptr);

  // Two-argument functions
  register_builtin(
      "pow", 2, 2, [](double x, double y) { return std::pow(x, y); },
      [](std::complex<double> x, std::complex<double> y) { return std::pow(x, y); });
  register_builtin("pwr", 2, 2, pwr, nullptr);
  register_builtin("pwrs", 2, 2, pwrs, nullptr);
  register_builtin("hypot", 2, 2, hypot_func, nullptr);
  register_builtin("atan2", 2, 2, atan2_func, nullptr);
  register_builtin("fmod", 2, 2, [](double x, double y) { return std::fmod(x, y); }, nullptr);

  // Variadic functions
  register_builtin("min", 1, -1, minN, nullptr);
  register_builtin("max", 1, -1, maxN, nullptr);
  register_builtin("limit", 3, 3, RealFuncN([](const std::vector<double>& args) {
                     return limit(args[0], args[1], args[2]);
                   }),
                   nullptr);

  // Complex-specific functions
  register_builtin(
      "real", 1, 1, [](double x) { return x; },
      [](std::complex<double> x) { return std::complex<double>(x.real(), 0); });
  register_builtin(
      "imag", 1, 1, [](double) { return 0.0; },
      [](std::complex<double> x) { return std::complex<double>(x.imag(), 0); });
  register_builtin(
      "mag", 1, 1, [](double x) { return std::fabs(x); },
      [](std::complex<double> x) { return std::complex<double>(std::abs(x), 0); });
  register_builtin(
      "phase", 1, 1, [](double) { return 0.0; },
      [](std::complex<double> x) { return std::complex<double>(std::arg(x), 0); });
  register_builtin(
      "conj", 1, 1, [](double x) { return x; },
      [](std::complex<double> x) { return std::conj(x); });
}

void FunctionRegistry::register_builtin(const std::string& name, int minArgs, int maxArgs,
                                        RealFunc1 realImpl, ComplexFunc1 complexImpl) {
  BuiltinFunction func;
  func.name = name;
  func.minArgs = minArgs;
  func.maxArgs = maxArgs;
  func.realImpl = realImpl;
  func.complexImpl = complexImpl ? complexImpl : ComplexFunc1(nullptr);
  func.supportsComplex = (complexImpl != nullptr);
  builtins_[normalize(name)] = std::move(func);
}

void FunctionRegistry::register_builtin(const std::string& name, int minArgs, int maxArgs,
                                        RealFunc2 realImpl, ComplexFunc2 complexImpl) {
  BuiltinFunction func;
  func.name = name;
  func.minArgs = minArgs;
  func.maxArgs = maxArgs;
  func.realImpl = realImpl;
  func.complexImpl = complexImpl ? complexImpl : ComplexFunc2(nullptr);
  func.supportsComplex = (complexImpl != nullptr);
  builtins_[normalize(name)] = std::move(func);
}

void FunctionRegistry::register_builtin(const std::string& name, int minArgs, int maxArgs,
                                        RealFuncN realImpl, ComplexFuncN complexImpl) {
  BuiltinFunction func;
  func.name = name;
  func.minArgs = minArgs;
  func.maxArgs = maxArgs;
  func.realImpl = realImpl;
  func.complexImpl = complexImpl ? complexImpl : ComplexFuncN(nullptr);
  func.supportsComplex = (complexImpl != nullptr);
  builtins_[normalize(name)] = std::move(func);
}

void FunctionRegistry::register_user_function(std::unique_ptr<UserFunction> func) {
  std::string name = normalize(func->name());
  userFunctions_[name] = std::move(func);
}

void FunctionRegistry::unregister_user_function(std::string_view name) {
  userFunctions_.erase(normalize(name));
}

const BuiltinFunction* FunctionRegistry::lookup_builtin(std::string_view name) const {
  auto it = builtins_.find(normalize(name));
  return (it != builtins_.end()) ? &it->second : nullptr;
}

const UserFunction* FunctionRegistry::lookup_user(std::string_view name) const {
  auto it = userFunctions_.find(normalize(name));
  return (it != userFunctions_.end()) ? it->second.get() : nullptr;
}

bool FunctionRegistry::exists(std::string_view name) const {
  std::string norm = normalize(name);
  return builtins_.find(norm) != builtins_.end() ||
         userFunctions_.find(norm) != userFunctions_.end();
}

bool FunctionRegistry::is_builtin(std::string_view name) const {
  return builtins_.find(normalize(name)) != builtins_.end();
}

bool FunctionRegistry::is_user_defined(std::string_view name) const {
  return userFunctions_.find(normalize(name)) != userFunctions_.end();
}

double FunctionRegistry::evaluate_real_builtin(std::string_view name,
                                               const std::vector<double>& args) const {
  const BuiltinFunction* func = lookup_builtin(name);
  if (!func) {
    throw std::runtime_error("Unknown function: " + std::string{name});
  }

  int argCount = static_cast<int>(args.size());
  if (argCount < func->minArgs) {
    throw std::runtime_error("Too few arguments for function " + std::string{name});
  }
  if (func->maxArgs >= 0 && argCount > func->maxArgs) {
    throw std::runtime_error("Too many arguments for function " + std::string{name});
  }

  if (std::holds_alternative<RealFunc1>(func->realImpl)) {
    return std::get<RealFunc1>(func->realImpl)(args[0]);
  } else if (std::holds_alternative<RealFunc2>(func->realImpl)) {
    return std::get<RealFunc2>(func->realImpl)(args[0], args[1]);
  } else {
    return std::get<RealFuncN>(func->realImpl)(args);
  }
}

std::complex<double> FunctionRegistry::evaluate_complex_builtin(
    std::string_view name, const std::vector<std::complex<double>>& args) const {
  const BuiltinFunction* func = lookup_builtin(name);
  if (!func) {
    throw std::runtime_error("Unknown function: " + std::string{name});
  }
  if (!func->supportsComplex) {
    throw std::runtime_error("Function " + std::string{name} + " does not support complex numbers");
  }

  int argCount = static_cast<int>(args.size());
  if (argCount < func->minArgs) {
    throw std::runtime_error("Too few arguments for function " + std::string{name});
  }
  if (func->maxArgs >= 0 && argCount > func->maxArgs) {
    throw std::runtime_error("Too many arguments for function " + std::string{name});
  }

  if (std::holds_alternative<ComplexFunc1>(func->complexImpl)) {
    return std::get<ComplexFunc1>(func->complexImpl)(args[0]);
  } else if (std::holds_alternative<ComplexFunc2>(func->complexImpl)) {
    return std::get<ComplexFunc2>(func->complexImpl)(args[0], args[1]);
  } else {
    return std::get<ComplexFuncN>(func->complexImpl)(args);
  }
}

std::vector<std::string> FunctionRegistry::all_function_names() const {
  std::vector<std::string> names;
  for (const auto& [name, func] : builtins_) {
    names.push_back(func.name);
  }
  for (const auto& [name, func] : userFunctions_) {
    names.push_back(func->name());
  }
  return names;
}

std::string FunctionRegistry::normalize(std::string_view name) {
  std::string result{name};
  std::transform(result.begin(), result.end(), result.begin(),
                 [](unsigned char c) { return std::tolower(c); });
  return result;
}

}  // namespace spice_expr
