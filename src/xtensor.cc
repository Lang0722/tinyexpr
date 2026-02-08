#include "spice_expr/array/xtensor.h"

#include <algorithm>
#include <cmath>
#include <utility>

namespace spice_expr {

// Helper function implementations

namespace detail {

RealXTensor fmod(const RealXTensor& a, const RealXTensor& b) {
  return tt::detail::binary_op(a, b,
                               [](double x, double y) { return std::fmod(x, y); });
}

RealXTensor cast_to_double(const tt::tensor<uint8_t>& t) {
  RealXTensor result(t.shape());
  for (size_t i = 0; i < t.size(); ++i) {
    result.flat(i) = static_cast<double>(t.flat(i));
  }
  return result;
}

ComplexXTensor cast_to_complex(const RealXTensor& t) {
  ComplexXTensor result(t.shape());
  for (size_t i = 0; i < t.size(); ++i) {
    result.flat(i) = std::complex<double>(t.flat(i), 0.0);
  }
  return result;
}

RealXTensor extract_real(const ComplexXTensor& t) {
  RealXTensor result(t.shape());
  for (size_t i = 0; i < t.size(); ++i) {
    result.flat(i) = t.flat(i).real();
  }
  return result;
}

RealXTensor extract_imag(const ComplexXTensor& t) {
  RealXTensor result(t.shape());
  for (size_t i = 0; i < t.size(); ++i) {
    result.flat(i) = t.flat(i).imag();
  }
  return result;
}

}  // namespace detail

// XTensor implementation

XTensor::XTensor() : data_(RealXTensor(tt::shape_t{0})) {}

XTensor::~XTensor() = default;

XTensor::XTensor(XTensor&& other) noexcept = default;
XTensor& XTensor::operator=(XTensor&& other) noexcept = default;

XTensor::XTensor(const XTensor& other) = default;

XTensor& XTensor::operator=(const XTensor& other) = default;

XTensor::XTensor(const std::vector<double>& vec) {
  RealXTensor t(tt::shape_t{vec.size()});
  std::copy(vec.begin(), vec.end(), t.data());
  data_ = std::move(t);
}

XTensor::XTensor(const std::vector<std::complex<double>>& vec) {
  ComplexXTensor t(tt::shape_t{vec.size()});
  std::copy(vec.begin(), vec.end(), t.data());
  data_ = std::move(t);
}

XTensor::XTensor(std::initializer_list<double> values) : data_(RealXTensor(values)) {}

XTensor::XTensor(RealXTensor arr) : data_(std::move(arr)) {}

XTensor::XTensor(ComplexXTensor arr) : data_(std::move(arr)) {}

bool XTensor::is_real() const { return std::holds_alternative<RealXTensor>(data_); }

bool XTensor::is_complex() const { return std::holds_alternative<ComplexXTensor>(data_); }

bool XTensor::is_scalar() const { return size() == 1; }

size_t XTensor::size() const {
  return std::visit([](const auto& arr) { return arr.size(); }, data_);
}

size_t XTensor::ndim() const {
  return std::visit([](const auto& arr) { return arr.ndim(); }, data_);
}

std::vector<size_t> XTensor::shape() const {
  return std::visit(
      [](const auto& arr) {
        const auto& s = arr.shape();
        return std::vector<size_t>(s.begin(), s.end());
      },
      data_);
}

double XTensor::get_real_at(size_t index) const {
  if (is_real()) {
    return real().flat(index);
  }
  return std::real(complex().flat(index));
}

std::complex<double> XTensor::get_complex_at(size_t index) const {
  if (is_real()) {
    return {real().flat(index), 0.0};
  }
  return complex().flat(index);
}

XTensor XTensor::to_complex() const {
  if (is_complex()) {
    return *this;
  }
  return XTensor(detail::cast_to_complex(real()));
}

double XTensor::sum_real() const {
  if (is_real()) {
    return tt::sum(real());
  }
  return std::real(tt::sum(complex()));
}

std::complex<double> XTensor::sum_complex() const {
  if (is_real()) {
    return std::complex<double>(tt::sum(real()), 0.0);
  }
  return tt::sum(complex());
}

double XTensor::mean_real() const {
  if (is_real()) {
    return tt::mean(real());
  }
  return std::real(tt::mean(complex()));
}

double XTensor::min_real() const {
  if (is_real()) {
    return tt::min(real());
  }
  throw XTensorError("min() not defined for complex arrays");
}

double XTensor::max_real() const {
  if (is_real()) {
    return tt::max(real());
  }
  throw XTensorError("max() not defined for complex arrays");
}

RealXTensor& XTensor::real() {
  if (!is_real()) {
    throw XTensorError("Cannot access complex array as real");
  }
  return std::get<RealXTensor>(data_);
}

const RealXTensor& XTensor::real() const {
  if (!is_real()) {
    throw XTensorError("Cannot access complex array as real");
  }
  return std::get<RealXTensor>(data_);
}

ComplexXTensor& XTensor::complex() {
  if (!is_complex()) {
    throw XTensorError("Cannot access real array as complex");
  }
  return std::get<ComplexXTensor>(data_);
}

const ComplexXTensor& XTensor::complex() const {
  if (!is_complex()) {
    throw XTensorError("Cannot access real array as complex");
  }
  return std::get<ComplexXTensor>(data_);
}

// Static scalar factory functions (1-D tensors with shape {1})
XTensor XTensor::scalar(double value) { return XTensor(RealXTensor({value})); }

XTensor XTensor::scalar(std::complex<double> value) {
  return XTensor(ComplexXTensor({value}));
}

// Factory functions

XTensor linspace(double start, double stop, size_t num) {
  return XTensor(tt::linspace<double>(start, stop, num));
}

XTensor arange(double start, double stop, double step) {
  return XTensor(tt::arange<double>(start, stop, step));
}

XTensor zeros(size_t n) { return XTensor(tt::zeros<double>(tt::shape_t{n})); }

XTensor zeros(const std::vector<size_t>& shape) {
  return XTensor(tt::zeros<double>(tt::shape_t(shape.begin(), shape.end())));
}

XTensor ones(size_t n) { return XTensor(tt::ones<double>(tt::shape_t{n})); }

XTensor ones(const std::vector<size_t>& shape) {
  return XTensor(tt::ones<double>(tt::shape_t(shape.begin(), shape.end())));
}

}  // namespace spice_expr
