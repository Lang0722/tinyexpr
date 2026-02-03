#ifndef SPICE_EXPR_ARRAY_ARRAY_VALUE_H
#define SPICE_EXPR_ARRAY_ARRAY_VALUE_H

#include <complex>
#include <stdexcept>
#include <variant>
#include <vector>

namespace spice_expr {

class ArrayError : public std::runtime_error {
 public:
  explicit ArrayError(const std::string& msg) : std::runtime_error(msg) {}
};

class ArrayValue {
 public:
  using RealArray = std::vector<double>;
  using ComplexArray = std::vector<std::complex<double>>;

  ArrayValue() = default;
  explicit ArrayValue(RealArray values);
  explicit ArrayValue(ComplexArray values);

  bool is_real() const { return std::holds_alternative<RealArray>(data_); }
  bool is_complex() const { return std::holds_alternative<ComplexArray>(data_); }
  bool is_empty() const;
  size_t size() const;

  double get_real_at(size_t index) const;
  std::complex<double> get_complex_at(size_t index) const;

  const RealArray& real_data() const;
  const ComplexArray& complex_data() const;

  ArrayValue to_complex() const;

  static double sum(const ArrayValue& arr);
  static double avg(const ArrayValue& arr);
  static double length(const ArrayValue& arr);

 private:
  std::variant<RealArray, ComplexArray> data_;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_ARRAY_ARRAY_VALUE_H
