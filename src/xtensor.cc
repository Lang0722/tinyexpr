#include "spice_expr/array/xtensor.h"
#include "spice_expr/array/xtensor_impl.h"

namespace spice_expr {

// XTensor implementation

XTensor::XTensor() : impl_(make_real_impl()) {}

XTensor::~XTensor() = default;

XTensor::XTensor(XTensor&& other) noexcept = default;
XTensor& XTensor::operator=(XTensor&& other) noexcept = default;

XTensor::XTensor(const XTensor& other) : impl_(other.impl_->clone()) {}

XTensor& XTensor::operator=(const XTensor& other) {
  if (this != &other) {
    impl_ = other.impl_->clone();
  }
  return *this;
}

XTensor::XTensor(const std::vector<double>& vec)
    : impl_(make_real_impl(RealXTensor(xt::adapt(vec, {vec.size()})))) {}

XTensor::XTensor(const std::vector<std::complex<double>>& vec)
    : impl_(make_complex_impl(ComplexXTensor(xt::adapt(vec, {vec.size()})))) {}

XTensor::XTensor(std::initializer_list<double> values)
    : impl_(make_real_impl(
          RealXTensor(xt::adapt(std::vector<double>(values), {values.size()})))) {}

// Private constructor for factory functions
XTensor::XTensor(std::unique_ptr<XTensorImplBase> impl) : impl_(std::move(impl)) {}

bool XTensor::is_real() const { return impl_->is_real(); }

bool XTensor::is_complex() const { return impl_->is_complex(); }

bool XTensor::is_scalar() const { return impl_->ndim() == 0; }

size_t XTensor::size() const { return impl_->size(); }

size_t XTensor::ndim() const { return impl_->ndim(); }

std::vector<size_t> XTensor::shape() const { return impl_->shape(); }

double XTensor::get_real_at(size_t index) const {
  if (is_real()) {
    return impl_->as_real().flat(index);
  } else {
    return std::real(impl_->as_complex().flat(index));
  }
}

std::complex<double> XTensor::get_complex_at(size_t index) const {
  if (is_real()) {
    return std::complex<double>(impl_->as_real().flat(index), 0.0);
  } else {
    return impl_->as_complex().flat(index);
  }
}

XTensor XTensor::to_complex() const {
  if (is_complex()) {
    return *this;
  }
  XTensor result;
  result.impl_ = make_complex_impl(ComplexXTensor(xt::cast<std::complex<double>>(impl_->as_real())));
  return result;
}

double XTensor::sum_real() const {
  if (is_real()) {
    return xt::sum(impl_->as_real())();
  } else {
    return std::real(xt::sum(impl_->as_complex())());
  }
}

std::complex<double> XTensor::sum_complex() const {
  if (is_real()) {
    return std::complex<double>(xt::sum(impl_->as_real())(), 0.0);
  } else {
    return xt::sum(impl_->as_complex())();
  }
}

double XTensor::mean_real() const {
  if (is_real()) {
    return xt::mean(impl_->as_real())();
  } else {
    return std::real(xt::mean(impl_->as_complex())());
  }
}

double XTensor::min_real() const {
  if (is_real()) {
    return xt::amin(impl_->as_real())();
  } else {
    throw XTensorError("min() not defined for complex arrays");
  }
}

double XTensor::max_real() const {
  if (is_real()) {
    return xt::amax(impl_->as_real())();
  } else {
    throw XTensorError("max() not defined for complex arrays");
  }
}

// Static scalar factory functions (0-dimensional tensors)
XTensor XTensor::scalar(double value) {
  // Create 0-D tensor with empty shape {}
  RealXTensor arr = RealXTensor::from_shape({});
  arr() = value;
  return XTensor(make_real_impl(std::move(arr)));
}

XTensor XTensor::scalar(std::complex<double> value) {
  // Create 0-D tensor with empty shape {}
  ComplexXTensor arr = ComplexXTensor::from_shape({});
  arr() = value;
  return XTensor(make_complex_impl(std::move(arr)));
}

// Factory functions

XTensor linspace(double start, double stop, size_t num) {
  return XTensor(make_real_impl(RealXTensor(xt::linspace<double>(start, stop, num))));
}

XTensor arange(double start, double stop, double step) {
  return XTensor(make_real_impl(RealXTensor(xt::arange<double>(start, stop, step))));
}

XTensor zeros(size_t n) {
  return XTensor(make_real_impl(RealXTensor(xt::zeros<double>({n}))));
}

XTensor zeros(const std::vector<size_t>& shape) {
  return XTensor(make_real_impl(RealXTensor(xt::zeros<double>(shape))));
}

XTensor ones(size_t n) {
  return XTensor(make_real_impl(RealXTensor(xt::ones<double>({n}))));
}

XTensor ones(const std::vector<size_t>& shape) {
  return XTensor(make_real_impl(RealXTensor(xt::ones<double>(shape))));
}

// Internal helper for ArrayEvaluator
XTensor make_xtensor_from_impl(std::unique_ptr<XTensorImplBase> impl) {
  return XTensor(std::move(impl));
}

}  // namespace spice_expr
