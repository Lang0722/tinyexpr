#ifndef SPICE_EXPR_ARRAY_XTENSOR_IMPL_H
#define SPICE_EXPR_ARRAY_XTENSOR_IMPL_H

// Internal header for files that need direct access to xtensor arrays.
// This header is NOT part of the public API.
// Normal users should only include xtensor.h

#include <complex>
#include <memory>
#include <stdexcept>
#include <vector>

// xtensor master uses modular headers in subdirectories
#include <xtensor/containers/xarray.hpp>
#include <xtensor/containers/xadapt.hpp>
#include <xtensor/generators/xbuilder.hpp>
#include <xtensor/core/xmath.hpp>
#include <xtensor/views/xview.hpp>
#include <xtensor/io/xio.hpp>
#include <xtensor/misc/xcomplex.hpp>

namespace spice_expr {

// Type aliases for xtensor arrays
using RealXTensor = xt::xarray<double>;
using ComplexXTensor = xt::xarray<std::complex<double>>;

// Abstract base class for type-erased array storage
class XTensorImplBase {
 public:
  virtual ~XTensorImplBase() = default;
  virtual bool is_real() const = 0;
  virtual bool is_complex() const = 0;
  virtual size_t size() const = 0;
  virtual size_t ndim() const = 0;
  virtual std::vector<size_t> shape() const = 0;
  virtual std::unique_ptr<XTensorImplBase> clone() const = 0;

  // Access as specific type (throws if wrong type)
  virtual RealXTensor& as_real() {
    throw std::runtime_error("Cannot access complex array as real");
  }
  virtual const RealXTensor& as_real() const {
    throw std::runtime_error("Cannot access complex array as real");
  }
  virtual ComplexXTensor& as_complex() {
    throw std::runtime_error("Cannot access real array as complex");
  }
  virtual const ComplexXTensor& as_complex() const {
    throw std::runtime_error("Cannot access real array as complex");
  }
};

// Specialization for real arrays
class RealXTensorImpl : public XTensorImplBase {
 public:
  RealXTensor data;

  RealXTensorImpl() : data(RealXTensor::from_shape({0})) {}  // explicitly empty
  explicit RealXTensorImpl(RealXTensor arr) : data(std::move(arr)) {}

  bool is_real() const override { return true; }
  bool is_complex() const override { return false; }
  size_t size() const override { return data.size(); }
  size_t ndim() const override { return data.dimension(); }

  std::vector<size_t> shape() const override {
    const auto& s = data.shape();
    return std::vector<size_t>(s.begin(), s.end());
  }

  std::unique_ptr<XTensorImplBase> clone() const override {
    return std::make_unique<RealXTensorImpl>(data);
  }

  RealXTensor& as_real() override { return data; }
  const RealXTensor& as_real() const override { return data; }
};

// Specialization for complex arrays
class ComplexXTensorImpl : public XTensorImplBase {
 public:
  ComplexXTensor data;

  ComplexXTensorImpl() : data(ComplexXTensor::from_shape({0})) {}  // explicitly empty
  explicit ComplexXTensorImpl(ComplexXTensor arr) : data(std::move(arr)) {}

  bool is_real() const override { return false; }
  bool is_complex() const override { return true; }
  size_t size() const override { return data.size(); }
  size_t ndim() const override { return data.dimension(); }

  std::vector<size_t> shape() const override {
    const auto& s = data.shape();
    return std::vector<size_t>(s.begin(), s.end());
  }

  std::unique_ptr<XTensorImplBase> clone() const override {
    return std::make_unique<ComplexXTensorImpl>(data);
  }

  ComplexXTensor& as_complex() override { return data; }
  const ComplexXTensor& as_complex() const override { return data; }
};

// Helper to create the right impl type
inline std::unique_ptr<XTensorImplBase> make_real_impl() {
  return std::make_unique<RealXTensorImpl>();
}

inline std::unique_ptr<XTensorImplBase> make_real_impl(RealXTensor arr) {
  return std::make_unique<RealXTensorImpl>(std::move(arr));
}

inline std::unique_ptr<XTensorImplBase> make_complex_impl() {
  return std::make_unique<ComplexXTensorImpl>();
}

inline std::unique_ptr<XTensorImplBase> make_complex_impl(ComplexXTensor arr) {
  return std::make_unique<ComplexXTensorImpl>(std::move(arr));
}

}  // namespace spice_expr

#endif  // SPICE_EXPR_ARRAY_XTENSOR_IMPL_H
