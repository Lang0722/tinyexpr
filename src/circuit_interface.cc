#include "spice_expr/circuit/circuit_interface.h"

#include <stdexcept>

namespace spice_expr {

// MockCircuitInterface

MockCircuitInterface::MockCircuitInterface() : analysisType_(AnalysisType::DC) {}

void MockCircuitInterface::set_node_voltage(const std::string& node, double value) {
  nodeVoltages_[node] = std::complex<double>(value, 0.0);
}

void MockCircuitInterface::set_node_voltage(const std::string& node, std::complex<double> value) {
  nodeVoltages_[node] = value;
}

void MockCircuitInterface::set_device_current(const std::string& device, double value) {
  deviceCurrents_[device] = std::complex<double>(value, 0.0);
}

void MockCircuitInterface::set_device_current(const std::string& device,
                                              std::complex<double> value) {
  deviceCurrents_[device] = value;
}

void MockCircuitInterface::set_analysis_type(AnalysisType type) {
  analysisType_ = type;
}

double MockCircuitInterface::get_node_voltage_real(const std::string& node) const {
  auto it = nodeVoltages_.find(node);
  if (it == nodeVoltages_.end()) {
    throw std::runtime_error("Node voltage not found: " + node);
  }
  return it->second.real();
}

double MockCircuitInterface::get_differential_voltage_real(const std::string& node1,
                                                           const std::string& node2) const {
  return get_node_voltage_real(node1) - get_node_voltage_real(node2);
}

double MockCircuitInterface::get_device_current_real(const std::string& device) const {
  auto it = deviceCurrents_.find(device);
  if (it == deviceCurrents_.end()) {
    throw std::runtime_error("Device current not found: " + device);
  }
  return it->second.real();
}

std::complex<double> MockCircuitInterface::get_node_voltage_complex(const std::string& node) const {
  auto it = nodeVoltages_.find(node);
  if (it == nodeVoltages_.end()) {
    throw std::runtime_error("Node voltage not found: " + node);
  }
  return it->second;
}

std::complex<double> MockCircuitInterface::get_differential_voltage_complex(
    const std::string& node1, const std::string& node2) const {
  return get_node_voltage_complex(node1) - get_node_voltage_complex(node2);
}

std::complex<double> MockCircuitInterface::get_device_current_complex(
    const std::string& device) const {
  auto it = deviceCurrents_.find(device);
  if (it == deviceCurrents_.end()) {
    throw std::runtime_error("Device current not found: " + device);
  }
  return it->second;
}

void MockCircuitInterface::clear() {
  nodeVoltages_.clear();
  deviceCurrents_.clear();
}

// NullCircuitInterface

double NullCircuitInterface::get_node_voltage_real(const std::string& node) const {
  throw std::runtime_error("No circuit interface: cannot get V(" + node + ")");
}

double NullCircuitInterface::get_differential_voltage_real(const std::string& node1,
                                                           const std::string& node2) const {
  throw std::runtime_error("No circuit interface: cannot get V(" + node1 + ", " + node2 + ")");
}

double NullCircuitInterface::get_device_current_real(const std::string& device) const {
  throw std::runtime_error("No circuit interface: cannot get I(" + device + ")");
}

std::complex<double> NullCircuitInterface::get_node_voltage_complex(const std::string& node) const {
  throw std::runtime_error("No circuit interface: cannot get V(" + node + ")");
}

std::complex<double> NullCircuitInterface::get_differential_voltage_complex(
    const std::string& node1, const std::string& node2) const {
  throw std::runtime_error("No circuit interface: cannot get V(" + node1 + ", " + node2 + ")");
}

std::complex<double> NullCircuitInterface::get_device_current_complex(
    const std::string& device) const {
  throw std::runtime_error("No circuit interface: cannot get I(" + device + ")");
}

}  // namespace spice_expr
