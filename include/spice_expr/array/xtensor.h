#ifndef SPICE_EXPR_ARRAY_XTENSOR_H
#define SPICE_EXPR_ARRAY_XTENSOR_H

#include <complex>
#include <memory>
#include <stdexcept>
#include <vector>

namespace spice_expr {

class XTensorError : public std::runtime_error {
 public:
  explicit XTensorError(const std::string& msg) : std::runtime_error(msg) {}
};

// Forward declaration of implementation base class (hides xtensor from header)
class XTensorImplBase;

// Type-erased wrapper for xtensor arrays supporting both real and complex types
// Uses PIMPL to avoid xtensor template instantiation issues in headers
class XTensor {
 public:
  XTensor();
  ~XTensor();

  // Move operations
  XTensor(XTensor&& other) noexcept;
  XTensor& operator=(XTensor&& other) noexcept;

  // Copy operations
  XTensor(const XTensor& other);
  XTensor& operator=(const XTensor& other);

  // Construction from std::vector (1D arrays)
  explicit XTensor(const std::vector<double>& vec);
  explicit XTensor(const std::vector<std::complex<double>>& vec);

  // Construction from initializer list
  XTensor(std::initializer_list<double> values);

  // Scalar construction (0-dimensional tensors)
  static XTensor scalar(double value);
  static XTensor scalar(std::complex<double> value);

  // Type queries
  bool is_real() const;
  bool is_complex() const;
  bool is_scalar() const;  // True if 0-D tensor (single element that broadcasts)
  bool is_empty() const { return size() == 0; }

  // Shape information
  size_t size() const;
  size_t ndim() const;
  std::vector<size_t> shape() const;

  // Element access (1D)
  double get_real_at(size_t index) const;
  std::complex<double> get_complex_at(size_t index) const;

  // Type conversion
  XTensor to_complex() const;

  // Reduction operations (return scalars)
  double sum_real() const;
  std::complex<double> sum_complex() const;
  double mean_real() const;
  double min_real() const;
  double max_real() const;

  // Access to implementation (for internal use by ArrayEvaluator)
  XTensorImplBase* impl() { return impl_.get(); }
  const XTensorImplBase* impl() const { return impl_.get(); }

 private:
  // Private constructor for factory functions
  explicit XTensor(std::unique_ptr<XTensorImplBase> impl);

  std::unique_ptr<XTensorImplBase> impl_;

  // Factory functions need access to private constructor
  friend XTensor linspace(double start, double stop, size_t num);
  friend XTensor arange(double start, double stop, double step);
  friend XTensor zeros(size_t n);
  friend XTensor zeros(const std::vector<size_t>& shape);
  friend XTensor ones(size_t n);
  friend XTensor ones(const std::vector<size_t>& shape);
  friend XTensor make_scalar_real(double value);
  friend XTensor make_scalar_complex(std::complex<double> value);

  // Internal helper for creating XTensor from impl (used by ArrayEvaluator)
  friend XTensor make_xtensor_from_impl(std::unique_ptr<XTensorImplBase> impl);
};

// Factory functions for array creation
XTensor linspace(double start, double stop, size_t num);
XTensor arange(double start, double stop, double step = 1.0);
XTensor zeros(size_t n);
XTensor zeros(const std::vector<size_t>& shape);
XTensor ones(size_t n);
XTensor ones(const std::vector<size_t>& shape);

// Internal helper for ArrayEvaluator (declared here, defined in xtensor.cc)
XTensor make_xtensor_from_impl(std::unique_ptr<XTensorImplBase> impl);

}  // namespace spice_expr

#endif  // SPICE_EXPR_ARRAY_XTENSOR_H
