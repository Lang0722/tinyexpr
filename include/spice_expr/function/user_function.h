#ifndef SPICE_EXPR_FUNCTION_USER_FUNCTION_H
#define SPICE_EXPR_FUNCTION_USER_FUNCTION_H

#include <string>
#include <vector>

namespace spice_expr {

class ExprNode;

class UserFunction {
 public:
  UserFunction(std::string name, std::vector<std::string> parameters, ExprNode* body);

  const std::string& name() const { return name_; }
  const std::vector<std::string>& parameters() const { return parameters_; }
  size_t parameter_count() const { return parameters_.size(); }
  ExprNode* body() const { return body_; }

 private:
  std::string name_;
  std::vector<std::string> parameters_;
  ExprNode* body_;
};

}  // namespace spice_expr

#endif  // SPICE_EXPR_FUNCTION_USER_FUNCTION_H
