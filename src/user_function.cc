#include "spice_expr/function/user_function.h"

namespace spice_expr {

UserFunction::UserFunction(std::string name, std::vector<std::string> parameters, ExprNode* body)
    : name_(std::move(name)), parameters_(std::move(parameters)), body_(body) {}

}  // namespace spice_expr
