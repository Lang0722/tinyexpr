#ifndef SPICE_EXPR_FUNCTION_FUNCTION_REGISTRY_H
#define SPICE_EXPR_FUNCTION_FUNCTION_REGISTRY_H

#include <complex>
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <variant>
#include <vector>

#include "spice_expr/function/user_function.h"

namespace spice_expr {

using RealFunc1 = std::function<double(double)>;
using RealFunc2 = std::function<double(double, double)>;
using RealFuncN = std::function<double(const std::vector<double>&)>;

using ComplexFunc1 = std::function<std::complex<double>(std::complex<double>)>;
using ComplexFunc2 =
    std::function<std::complex<double>(std::complex<double>, std::complex<double>)>;
using ComplexFuncN = std::function<std::complex<double>(const std::vector<std::complex<double>>&)>;

struct BuiltinFunction {
  std::string name;
  int minArgs;
  int maxArgs;
  std::variant<RealFunc1, RealFunc2, RealFuncN> realImpl;
  std::variant<ComplexFunc1, ComplexFunc2, ComplexFuncN> complexImpl;
  bool supportsComplex;
};

class FunctionRegistry {
 public:
  FunctionRegistry();

  void register_builtin(const std::string& name, int minArgs, int maxArgs, RealFunc1 realImpl,
                        ComplexFunc1 complexImpl = nullptr);
  void register_builtin(const std::string& name, int minArgs, int maxArgs, RealFunc2 realImpl,
                        ComplexFunc2 complexImpl = nullptr);
  void register_builtin(const std::string& name, int minArgs, int maxArgs, RealFuncN realImpl,
                        ComplexFuncN complexImpl = nullptr);

  void register_user_function(std::unique_ptr<UserFunction> func);
  void unregister_user_function(std::string_view name);

  const BuiltinFunction* lookup_builtin(std::string_view name) const;
  const UserFunction* lookup_user(std::string_view name) const;

  bool exists(std::string_view name) const;
  bool is_builtin(std::string_view name) const;
  bool is_user_defined(std::string_view name) const;

  double evaluate_real_builtin(std::string_view name, const std::vector<double>& args) const;
  std::complex<double> evaluate_complex_builtin(
      std::string_view name, const std::vector<std::complex<double>>& args) const;

  std::vector<std::string> all_function_names() const;

 private:
  void register_builtins();
  static std::string normalize(std::string_view name);

  std::unordered_map<std::string, BuiltinFunction> builtins_;
  std::unordered_map<std::string, std::unique_ptr<UserFunction>> userFunctions_;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_FUNCTION_FUNCTION_REGISTRY_H
