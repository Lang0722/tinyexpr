#include "spice_expr/array/array_value.h"

#include <algorithm>
#include <numeric>

namespace spice_expr {

ArrayValue::ArrayValue(RealArray values) : data_(std::move(values)) {}

ArrayValue::ArrayValue(ComplexArray values) : data_(std::move(values)) {}

bool ArrayValue::is_empty() const {
  if (std::holds_alternative<RealArray>(data_)) {
    return std::get<RealArray>(data_).empty();
  }
  return std::get<ComplexArray>(data_).empty();
}

size_t ArrayValue::size() const {
  if (std::holds_alternative<RealArray>(data_)) {
    return std::get<RealArray>(data_).size();
  }
  return std::get<ComplexArray>(data_).size();
}

double ArrayValue::get_real_at(size_t index) const {
  if (std::holds_alternative<RealArray>(data_)) {
    const auto& arr = std::get<RealArray>(data_);
    if (index >= arr.size()) {
      throw ArrayError("Index out of bounds");
    }
    return arr[index];
  }
  const auto& arr = std::get<ComplexArray>(data_);
  if (index >= arr.size()) {
    throw ArrayError("Index out of bounds");
  }
  return arr[index].real();
}

std::complex<double> ArrayValue::get_complex_at(size_t index) const {
  if (std::holds_alternative<RealArray>(data_)) {
    const auto& arr = std::get<RealArray>(data_);
    if (index >= arr.size()) {
      throw ArrayError("Index out of bounds");
    }
    return {arr[index], 0.0};
  }
  const auto& arr = std::get<ComplexArray>(data_);
  if (index >= arr.size()) {
    throw ArrayError("Index out of bounds");
  }
  return arr[index];
}

const ArrayValue::RealArray& ArrayValue::real_data() const {
  if (!std::holds_alternative<RealArray>(data_)) {
    throw ArrayError("Array is not real");
  }
  return std::get<RealArray>(data_);
}

const ArrayValue::ComplexArray& ArrayValue::complex_data() const {
  if (!std::holds_alternative<ComplexArray>(data_)) {
    throw ArrayError("Array is not complex");
  }
  return std::get<ComplexArray>(data_);
}

ArrayValue ArrayValue::to_complex() const {
  if (std::holds_alternative<ComplexArray>(data_)) {
    return *this;
  }
  const auto& real = std::get<RealArray>(data_);
  ComplexArray complex(real.size());
  std::transform(real.begin(), real.end(), complex.begin(),
                 [](double v) { return std::complex<double>(v, 0.0); });
  return ArrayValue(std::move(complex));
}

double ArrayValue::sum(const ArrayValue& arr) {
  if (arr.is_empty())
    return 0.0;
  if (arr.is_real()) {
    const auto& data = arr.real_data();
    return std::accumulate(data.begin(), data.end(), 0.0);
  }
  const auto& data = arr.complex_data();
  return std::accumulate(data.begin(), data.end(), 0.0,
                         [](double s, const auto& v) { return s + v.real(); });
}

double ArrayValue::avg(const ArrayValue& arr) {
  if (arr.is_empty())
    throw ArrayError("Cannot compute average of empty array");
  return sum(arr) / static_cast<double>(arr.size());
}

double ArrayValue::length(const ArrayValue& arr) {
  return static_cast<double>(arr.size());
}

}  // namespace spice_expr
