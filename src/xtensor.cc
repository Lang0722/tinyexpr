#include "spice_expr/array/xtensor.h"

#include <utility>

namespace spice_expr {

// XTensor implementation

XTensor::XTensor() : data_(RealXTensor::from_shape({0})) {}

XTensor::~XTensor() = default;

XTensor::XTensor(XTensor&& other) noexcept = default;
XTensor& XTensor::operator=(XTensor&& other) noexcept = default;

XTensor::XTensor(const XTensor& other) = default;

XTensor& XTensor::operator=(const XTensor& other) = default;

XTensor::XTensor(const std::vector<double>& vec)
    : data_(RealXTensor(xt::adapt(vec, {vec.size()}))) {}

XTensor::XTensor(const std::vector<std::complex<double>>& vec)
    : data_(ComplexXTensor(xt::adapt(vec, {vec.size()}))) {}

XTensor::XTensor(std::initializer_list<double> values)
    : data_(RealXTensor(xt::adapt(std::vector<double>(values), {values.size()}))) {}

XTensor::XTensor(RealXTensor arr) : data_(std::move(arr)) {}

XTensor::XTensor(ComplexXTensor arr) : data_(std::move(arr)) {}

bool XTensor::is_real() const { return std::holds_alternative<RealXTensor>(data_); }

bool XTensor::is_complex() const { return std::holds_alternative<ComplexXTensor>(data_); }

bool XTensor::is_scalar() const { return ndim() == 0; }

size_t XTensor::size() const {
  return std::visit([](const auto& arr) { return arr.size(); }, data_);
}

size_t XTensor::ndim() const {
  return std::visit([](const auto& arr) { return arr.dimension(); }, data_);
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
  ComplexXTensor converted = xt::eval(xt::cast<std::complex<double>>(real()));
  return XTensor(std::move(converted));
}

double XTensor::sum_real() const {
  if (is_real()) {
    return xt::sum(real())();
  }
  return std::real(xt::sum(complex())());
}

std::complex<double> XTensor::sum_complex() const {
  if (is_real()) {
    return std::complex<double>(xt::sum(real())(), 0.0);
  }
  return xt::sum(complex())();
}

double XTensor::mean_real() const {
  if (is_real()) {
    return xt::mean(real())();
  }
  return std::real(xt::mean(complex())());
}

double XTensor::min_real() const {
  if (is_real()) {
    return xt::amin(real())();
  }
  throw XTensorError("min() not defined for complex arrays");
}

double XTensor::max_real() const {
  if (is_real()) {
    return xt::amax(real())();
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

// Static scalar factory functions (0-dimensional tensors)
XTensor XTensor::scalar(double value) {
  // Create 0-D tensor with empty shape {}
  RealXTensor arr = RealXTensor::from_shape({});
  arr() = value;
  return XTensor(std::move(arr));
}

XTensor XTensor::scalar(std::complex<double> value) {
  // Create 0-D tensor with empty shape {}
  ComplexXTensor arr = ComplexXTensor::from_shape({});
  arr() = value;
  return XTensor(std::move(arr));
}

// Factory functions

XTensor linspace(double start, double stop, size_t num) {
  return XTensor(RealXTensor(xt::linspace<double>(start, stop, num)));
}

XTensor arange(double start, double stop, double step) {
  return XTensor(RealXTensor(xt::arange<double>(start, stop, step)));
}

XTensor zeros(size_t n) { return XTensor(RealXTensor(xt::zeros<double>({n}))); }

XTensor zeros(const std::vector<size_t>& shape) {
  return XTensor(RealXTensor(xt::zeros<double>(shape)));
}

XTensor ones(size_t n) { return XTensor(RealXTensor(xt::ones<double>({n}))); }

XTensor ones(const std::vector<size_t>& shape) {
  return XTensor(RealXTensor(xt::ones<double>(shape)));
}

}  // namespace spice_expr
