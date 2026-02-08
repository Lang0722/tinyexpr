#ifndef SPICE_EXPR_ARRAY_XTENSOR_H
#define SPICE_EXPR_ARRAY_XTENSOR_H

#include <complex>
#include <stdexcept>
#include <variant>
#include <vector>

#include <tinytensor/tinytensor.hpp>

namespace spice_expr {

class XTensorError : public std::runtime_error {
 public:
  explicit XTensorError(const std::string& msg) : std::runtime_error(msg) {}
};

// Type aliases for tensor arrays (backed by tinytensor)
using RealXTensor = tt::tensor<double>;
using ComplexXTensor = tt::tensor<std::complex<double>>;

// Helper functions for operations not directly available in tinytensor
namespace detail {

// Element-wise floating-point modulo
RealXTensor fmod(const RealXTensor& a, const RealXTensor& b);

// Cast tensor<uint8_t> (comparison results) to tensor<double>
RealXTensor cast_to_double(const tt::tensor<uint8_t>& t);

// Cast real tensor to complex tensor
ComplexXTensor cast_to_complex(const RealXTensor& t);

// Extract real parts from complex tensor
RealXTensor extract_real(const ComplexXTensor& t);

// Extract imaginary parts from complex tensor
RealXTensor extract_imag(const ComplexXTensor& t);

}  // namespace detail

// Type-erased wrapper for tensor arrays supporting both real and complex types
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

  // Construction from tensor arrays
  explicit XTensor(RealXTensor arr);
  explicit XTensor(ComplexXTensor arr);

  // Scalar construction (1-D tensors with shape {1})
  static XTensor scalar(double value);
  static XTensor scalar(std::complex<double> value);

  // Type queries
  bool is_real() const;
  bool is_complex() const;
  bool is_scalar() const;  // True if single-element tensor that broadcasts
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

  // Access to stored arrays
  RealXTensor& real();
  const RealXTensor& real() const;
  ComplexXTensor& complex();
  const ComplexXTensor& complex() const;

 private:
  std::variant<RealXTensor, ComplexXTensor> data_;
};

// Factory functions for array creation
XTensor linspace(double start, double stop, size_t num);
XTensor arange(double start, double stop, double step = 1.0);
XTensor zeros(size_t n);
XTensor zeros(const std::vector<size_t>& shape);
XTensor ones(size_t n);
XTensor ones(const std::vector<size_t>& shape);

}  // namespace spice_expr

#endif  // SPICE_EXPR_ARRAY_XTENSOR_H
