#ifndef SPICE_EXPR_CIRCUIT_CIRCUIT_INTERFACE_H
#define SPICE_EXPR_CIRCUIT_CIRCUIT_INTERFACE_H

#include <complex>
#include <optional>
#include <string>
#include <unordered_map>

namespace spice_expr {

enum class AnalysisType { DC, AC, Transient };

class CircuitInterface {
 public:
  virtual ~CircuitInterface() = default;

  virtual double get_node_voltage_real(const std::string& node) const = 0;
  virtual double get_differential_voltage_real(const std::string& node1,
                                               const std::string& node2) const = 0;
  virtual double get_device_current_real(const std::string& device) const = 0;

  virtual std::complex<double> get_node_voltage_complex(const std::string& node) const = 0;
  virtual std::complex<double> get_differential_voltage_complex(const std::string& node1,
                                                                const std::string& node2) const = 0;
  virtual std::complex<double> get_device_current_complex(const std::string& device) const = 0;

  virtual AnalysisType analysis_type() const = 0;
  virtual bool is_complex_analysis() const { return analysis_type() == AnalysisType::AC; }
};

class MockCircuitInterface : public CircuitInterface {
 public:
  MockCircuitInterface();

  void set_node_voltage(const std::string& node, double value);
  void set_node_voltage(const std::string& node, std::complex<double> value);
  void set_device_current(const std::string& device, double value);
  void set_device_current(const std::string& device, std::complex<double> value);
  void set_analysis_type(AnalysisType type);

  double get_node_voltage_real(const std::string& node) const override;
  double get_differential_voltage_real(const std::string& node1,
                                       const std::string& node2) const override;
  double get_device_current_real(const std::string& device) const override;

  std::complex<double> get_node_voltage_complex(const std::string& node) const override;
  std::complex<double> get_differential_voltage_complex(const std::string& node1,
                                                        const std::string& node2) const override;
  std::complex<double> get_device_current_complex(const std::string& device) const override;

  AnalysisType analysis_type() const override { return analysisType_; }

  void clear();

 private:
  std::unordered_map<std::string, std::complex<double>> nodeVoltages_;
  std::unordered_map<std::string, std::complex<double>> deviceCurrents_;
  AnalysisType analysisType_;
};

class NullCircuitInterface : public CircuitInterface {
 public:
  double get_node_voltage_real(const std::string& node) const override;
  double get_differential_voltage_real(const std::string& node1,
                                       const std::string& node2) const override;
  double get_device_current_real(const std::string& device) const override;

  std::complex<double> get_node_voltage_complex(const std::string& node) const override;
  std::complex<double> get_differential_voltage_complex(const std::string& node1,
                                                        const std::string& node2) const override;
  std::complex<double> get_device_current_complex(const std::string& device) const override;

  AnalysisType analysis_type() const override { return AnalysisType::DC; }
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_CIRCUIT_CIRCUIT_INTERFACE_H
