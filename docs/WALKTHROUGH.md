# SPICE Expression Module - Architecture Walkthrough

## Table of Contents

1. [Introduction](#introduction)
2. [Memory Management with Arena Allocation](#memory-management-with-arena-allocation)
3. [AST Node Hierarchy](#ast-node-hierarchy)
4. [Visitor Pattern Implementation](#visitor-pattern-implementation)
5. [Expression Evaluation](#expression-evaluation)
6. [Symbol Table and Scoping](#symbol-table-and-scoping)
7. [Dependency Analysis](#dependency-analysis)
8. [Parameter Flagging and Affected Parameters](#parameter-flagging-and-affected-parameters)
9. [Function Registry](#function-registry)
10. [Circuit Interface](#circuit-interface)
11. [Symbolic Differentiation](#symbolic-differentiation)
12. [Expression Simplification](#expression-simplification)
13. [Parser and Lexer](#parser-and-lexer)
14. [Expression Engine Facade](#expression-engine-facade)
15. [Design Decisions and Trade-offs](#design-decisions-and-trade-offs)

---

## Introduction

The spice_expr module is designed to handle mathematical expressions in SPICE circuit simulators. It supports:

- Parsing expressions like `vdd * 0.5 + V(out)` or `sin(2*pi*freq*time)`
- Evaluating expressions with parameter dependencies
- Symbolic differentiation for sensitivity analysis
- Circuit value references (node voltages, device currents)
- Hierarchical parameter scoping for subcircuits

The module is built for performance, using arena allocation and dual evaluation paths (real vs. complex) to minimize overhead.

---

## Memory Management with Arena Allocation

### The Problem
Expression trees can contain thousands of nodes. Using `shared_ptr` for each node would introduce significant overhead from reference counting and fragmentation.

### The Solution: ExprArena

```cpp
// include/spice_expr/ast/arena.h
class ExprArena {
    std::vector<std::unique_ptr<ExprNode>> nodes_;
public:
    template<typename T, typename... Args>
    T* make(Args&&... args) {
        auto node = std::make_unique<T>(std::forward<Args>(args)...);
        T* ptr = node.get();
        nodes_.push_back(std::move(node));
        return ptr;
    }
};
```

**Key Properties:**
- All nodes allocated through `arena.make<NodeType>(args...)`
- Arena owns all nodes; they're destroyed when the arena is destroyed
- Raw `ExprNode*` pointers used everywhere (no shared_ptr overhead)
- `clone(ExprArena&)` creates deep copies in a target arena

**Usage Pattern:**
```cpp
ExprArena arena;
auto* literal = arena.make<NumberLiteral>(3.14);
auto* ident = arena.make<Identifier>("x");
auto* expr = arena.make<BinaryOp>(BinaryOpType::Multiply, literal, ident);
// All three nodes valid until arena is destroyed
```

---

## AST Node Hierarchy

### Base Class: ExprNode

```cpp
// include/spice_expr/ast/expr_node.h
enum class NodeType {
    NumberLiteral, StringLiteral, ArrayLiteral,
    Identifier, CircuitNodeRef, CircuitCurrentRef,
    BinaryOp, UnaryOp,
    FunctionCall, ArrayIndex, TernaryConditional
};

class ExprNode {
public:
    virtual ~ExprNode() = default;
    virtual NodeType type() const = 0;
    virtual void accept(ExprVisitor& visitor) = 0;
    virtual void accept(ConstExprVisitor& visitor) const = 0;
    virtual ExprNode* clone(ExprArena& arena) const = 0;
    virtual size_t hash() const = 0;
    virtual bool equals(const ExprNode& other) const = 0;

    SourceLocation location;  // For error reporting
};
```

**Design Choice:** `NodeType` enum enables fast runtime type identification without RTTI overhead. Used for quick type checks in evaluation and simplification.

### Node Categories

**Literals** (`include/spice_expr/ast/literals.h`):
- `NumberLiteral` - Real or complex numbers
- `StringLiteral` - String values (for file paths, etc.)
- `ArrayLiteral` - Array of expressions

**References** (`include/spice_expr/ast/references.h`):
- `Identifier` - Parameter reference (e.g., `vdd`)
- `CircuitNodeRef` - Node voltage: `V(node)` or `V(n1, n2)`
- `CircuitCurrentRef` - Device current: `I(device)`

**Operations** (`include/spice_expr/ast/operations.h`):
- `BinaryOp` - Add, Subtract, Multiply, Divide, Power, comparisons, logical ops
- `UnaryOp` - Negate, Plus, LogicalNot

**Functions** (`include/spice_expr/ast/functions.h`):
- `FunctionCall` - Function invocation with arguments
- `ArrayIndex` - Array element access: `arr[i]`
- `TernaryConditional` - Conditional: `cond ? true_expr : false_expr`

---

## Visitor Pattern Implementation

### Why Visitor Pattern?

The visitor pattern separates algorithms from the AST structure. Adding new operations (evaluation, printing, differentiation) doesn't require modifying node classes.

### Dual Visitor Interfaces

```cpp
// include/spice_expr/visitor/visitor.h
class ExprVisitor {
public:
    virtual void visit(NumberLiteral& node) = 0;
    virtual void visit(StringLiteral& node) = 0;
    // ... one method per node type
};

class ConstExprVisitor {
public:
    virtual void visit(const NumberLiteral& node) = 0;
    virtual void visit(const StringLiteral& node) = 0;
    // ... one method per node type (const references)
};
```

**Why two interfaces?**
- `ExprVisitor` for transformations that modify nodes
- `ConstExprVisitor` for read-only operations (evaluation, printing)

### Node Accept Methods

```cpp
class NumberLiteral : public ExprNode {
    void accept(ExprVisitor& v) override { v.visit(*this); }
    void accept(ConstExprVisitor& v) const override { v.visit(*this); }
};
```

---

## Expression Evaluation

### Dual Evaluation Paths

**Problem:** Complex number operations are significantly slower than real operations. Most SPICE expressions only use real numbers.

**Solution:** Two separate evaluators with automatic selection.

```cpp
// Check if expression needs complex evaluation
bool requiresComplexEvaluation(const ExprNode& node);

// Fast path for real-only expressions
class RealEvaluator : public ConstExprVisitor {
    std::stack<double> stack_;
    // ...
};

// Complex number path
class ComplexEvaluator : public ConstExprVisitor {
    std::stack<std::complex<double>> stack_;
    // ...
};
```

### Stack-Based Evaluation

Evaluators use an internal stack instead of returning values:

```cpp
void RealEvaluator::visit(const BinaryOp& node) {
    node.left()->accept(*this);   // Pushes left result
    node.right()->accept(*this);  // Pushes right result
    double right = pop();
    double left = pop();

    switch (node.opType()) {
        case BinaryOpType::Add:
            stack_.push(left + right);
            break;
        // ...
    }
}
```

**Why stack-based?**
- Avoids function call overhead for returning values
- Natural fit for expression tree traversal
- Easy to extend for vector operations

### Identifier Evaluation

When evaluating identifiers (parameter references), the evaluator looks up the symbol and recursively evaluates its expression:

```cpp
void RealEvaluator::visit(const Identifier& node) {
    const SymbolEntry* entry = symbols_.lookup(node.name());
    if (!entry) {
        throw EvaluationError("Undefined parameter: " + node.name());
    }
    entry->expression->accept(*this);  // Recursively evaluate
}
```

---

## Symbol Table and Scoping

### Hierarchical Structure

SPICE subcircuits create nested parameter scopes. The symbol table supports this with parent pointers:

```cpp
class SymbolTable {
    SymbolTable* parent_;
    ScopingMode mode_;
    std::unordered_map<std::string, SymbolEntry> symbols_;
    std::unordered_map<std::string, ExprNode*> overrides_;
    std::unordered_map<std::string, ExprNode*> originalExpressions_;
};
```

### Scoping Modes

HSPICE has two scoping modes controlled by the `PARHIER` option:

**Local Mode (default):**
```cpp
const SymbolEntry* lookupLocal(const std::string& name) const {
    // 1. Check current scope
    auto it = symbols_.find(name);
    if (it != symbols_.end()) return &it->second;

    // 2. Check parent scope
    if (parent_) return parent_->lookupLocal(name);

    return nullptr;
}
```

**Global Mode:**
```cpp
const SymbolEntry* lookupGlobal(const std::string& name) const {
    // 1. Find root scope
    const SymbolTable* root = this;
    while (root->parent_) root = root->parent_;

    // 2. Check root first
    auto it = root->symbols_.find(name);
    if (it != root->symbols_.end()) return &it->second;

    // 3. Fall back to current scope
    it = symbols_.find(name);
    if (it != symbols_.end()) return &it->second;

    return nullptr;
}
```

### Override Mechanism

For parameter sweeps, values can be temporarily overridden:

```cpp
void setOverride(const std::string& name, ExprNode* expression) {
    // Save original if not already saved
    if (originalExpressions_.find(name) == originalExpressions_.end()) {
        if (auto* entry = lookupMutable(name)) {
            originalExpressions_[name] = entry->expression;
        }
    }
    overrides_[name] = expression;
}

void clearOverride(const std::string& name) {
    overrides_.erase(name);
    // Original expression remains in originalExpressions_ for potential future use
}
```

The original expression is preserved in `originalExpressions_` and restored when the override is cleared.

---

## Dependency Analysis

### Why Dependency Analysis?

Parameters can depend on other parameters:
```
.param a = 1
.param b = a + 1
.param c = b * 2
```

We need to evaluate them in the right order (a, then b, then c).

### Dependency Graph

```cpp
struct DependencyNode {
    std::string name;
    std::unordered_set<std::string> dependsOn;   // Parameters I need
    std::unordered_set<std::string> dependents;  // Parameters that need me
};

class DependencyGraph {
    std::unordered_map<std::string, DependencyNode> nodes_;

    void addDependency(const std::string& from, const std::string& to);
    bool hasCycle() const;
    std::optional<std::vector<std::string>> getCyclePath() const;
    std::vector<std::string> topologicalSort() const;
};
```

### Cycle Detection (DFS)

```cpp
bool DependencyGraph::hasCycle() const {
    std::unordered_set<std::string> visited, recursionStack;

    std::function<bool(const std::string&)> dfs = [&](const std::string& name) {
        visited.insert(name);
        recursionStack.insert(name);

        for (const auto& dep : nodes_.at(name).dependsOn) {
            if (recursionStack.count(dep)) return true;  // Cycle!
            if (!visited.count(dep) && dfs(dep)) return true;
        }

        recursionStack.erase(name);
        return false;
    };

    for (const auto& [name, _] : nodes_) {
        if (!visited.count(name) && dfs(name)) return true;
    }
    return false;
}
```

### Topological Sort (Kahn's Algorithm)

```cpp
std::vector<std::string> DependencyGraph::topologicalSort() const {
    std::unordered_map<std::string, int> inDegree;
    std::queue<std::string> ready;
    std::vector<std::string> order;

    // Initialize in-degrees
    for (const auto& [name, node] : nodes_) {
        inDegree[name] = node.dependsOn.size();
        if (inDegree[name] == 0) ready.push(name);
    }

    // Process nodes with no dependencies
    while (!ready.empty()) {
        std::string name = ready.front();
        ready.pop();
        order.push_back(name);

        for (const auto& dependent : nodes_.at(name).dependents) {
            if (--inDegree[dependent] == 0) {
                ready.push(dependent);
            }
        }
    }

    return order;
}
```

---

## Parameter Flagging and Affected Parameters

### Why Flagging?

In circuit simulation, certain parameters are often marked for special treatment:
- **Optimization targets**: Parameters to be adjusted during optimization
- **Sweep variables**: Parameters to vary in parametric analysis
- **Sensitivity targets**: Parameters for sensitivity analysis

The flagging system provides a generic mechanism to tag parameters with arbitrary labels.

### SymbolEntry Flags

Each `SymbolEntry` contains a set of string flags:

```cpp
struct SymbolEntry {
    std::string name;
    ExprNode* expression;
    std::set<std::string> flags;  // Arbitrary string tags
};
```

### Flag Management API

```cpp
class SymbolTable {
public:
    // Set a flag on a parameter
    void set_flag(std::string_view name, std::string_view flag);

    // Remove a specific flag
    void clear_flag(std::string_view name, std::string_view flag);

    // Remove all flags from a parameter
    void clear_all_flags(std::string_view name);

    // Check if a parameter has a specific flag
    bool has_flag(std::string_view name, std::string_view flag) const;

    // Get all flags for a parameter
    std::set<std::string> get_flags(std::string_view name) const;

    // Find all parameters with a specific flag (returns expression nodes directly)
    std::vector<ExprNode*> get_parameters_with_flag(std::string_view flag) const;
};
```

### Transitive Dependents

When a flagged parameter changes, all parameters that depend on it (directly or indirectly) are affected. The `DependencyGraph` provides transitive dependent queries:

```cpp
class DependencyGraph {
public:
    // Get all parameters that transitively depend on the given parameter
    std::set<std::string> get_transitive_dependents(std::string_view name) const;

    // Get transitive dependents for multiple parameters
    std::set<std::string> get_transitive_dependents(const std::set<std::string>& names) const;
};
```

**Implementation (BFS):**

```cpp
std::set<std::string> DependencyGraph::get_transitive_dependents(std::string_view name) const {
    std::set<std::string> result;
    std::queue<std::string> queue;

    // Start with direct dependents
    auto it = nodes_.find(std::string{name});
    if (it == nodes_.end()) return result;

    for (const auto& dep : it->second.dependents) {
        queue.push(dep);
    }

    // BFS traversal
    while (!queue.empty()) {
        std::string current = queue.front();
        queue.pop();

        if (result.count(current) > 0) continue;  // Already visited
        result.insert(current);

        auto nodeIt = nodes_.find(current);
        if (nodeIt != nodes_.end()) {
            for (const auto& dep : nodeIt->second.dependents) {
                if (result.count(dep) == 0) {
                    queue.push(dep);
                }
            }
        }
    }

    return result;
}
```

### Affected Parameters Query

The `ExpressionEngine` combines flagging and dependency analysis. APIs return `ExprNode*` instead of strings to avoid ambiguity with hierarchical scoping:

```cpp
class ExpressionEngine {
public:
    // Flag management
    void set_parameter_flag(const std::string& name, const std::string& flag);
    void clear_parameter_flag(const std::string& name, const std::string& flag);
    bool has_parameter_flag(const std::string& name, const std::string& flag) const;
    std::vector<ExprNode*> get_parameters_with_flag(const std::string& flag) const;

    // Get all parameters affected by changes to the given expression
    std::set<ExprNode*> get_affected_parameters(const ExprNode& expr) const;

    // Get flagged parameters plus all their transitive dependents
    std::set<ExprNode*> get_affected_parameters_by_flag(const std::string& flag) const;
};
```

### Usage Example

```cpp
ExpressionEngine engine;
ExprArena& arena = engine.arena();

// Define parameters with dependencies: a -> b -> c
auto* exprA = arena.make<NumberLiteral>(1.0);
auto* exprB = arena.make<BinaryOp>(BinaryOpType::Add,
    arena.make<Identifier>("a"), arena.make<NumberLiteral>(1.0));
auto* exprC = arena.make<BinaryOp>(BinaryOpType::Multiply,
    arena.make<Identifier>("b"), arena.make<NumberLiteral>(2.0));
engine.define_parameter("a", exprA);
engine.define_parameter("b", exprB);
engine.define_parameter("c", exprC);

engine.build_evaluation_order();

// Flag parameter as optimization target
engine.set_parameter_flag("a", "optimize");

// Get all affected parameters (b and c depend on a)
// Pass an expression containing the identifier "a"
auto affected = engine.get_affected_parameters(*arena.make<Identifier>("a"));
// affected = {exprB, exprC}

// Get affected by flag (includes flagged params + their dependents)
auto optimizeAffected = engine.get_affected_parameters_by_flag("optimize");
// optimizeAffected = {exprA, exprB, exprC}
```

### Use Cases

1. **Optimization**: Mark parameters for optimization, then identify which other parameters need re-evaluation when optimized values change.

2. **Sensitivity Analysis**: Flag parameters of interest, compute which outputs are affected.

3. **Incremental Evaluation**: When a parameter changes, only re-evaluate affected parameters instead of all parameters.

4. **Parameter Grouping**: Use flags to categorize parameters (e.g., "process", "voltage", "temperature").

---

## Function Registry

### Built-in Functions

```cpp
class FunctionRegistry {
    std::unordered_map<std::string, BuiltinFunction> builtins_;
    std::unordered_map<std::string, std::unique_ptr<UserFunction>> userFunctions_;

    void registerBuiltins() {
        // Math functions
        registerBuiltin("sin", 1, [](double x) { return std::sin(x); });
        registerBuiltin("cos", 1, [](double x) { return std::cos(x); });
        registerBuiltin("exp", 1, [](double x) { return std::exp(x); });
        registerBuiltin("log", 1, [](double x) { return std::log(x); });
        registerBuiltin("sqrt", 1, [](double x) { return std::sqrt(x); });

        // Multi-argument functions
        registerBuiltin("min", 2, [](double a, double b) { return std::min(a, b); });
        registerBuiltin("max", 2, [](double a, double b) { return std::max(a, b); });
        registerBuiltin("pow", 2, [](double a, double b) { return std::pow(a, b); });

        // SPICE-specific
        registerBuiltin("db", 1, [](double x) { return 20.0 * std::log10(std::abs(x)); });
    }
};
```

### User-Defined Functions

```cpp
class UserFunction {
    std::string name_;
    std::vector<std::string> parameters_;
    ExprNode* body_;  // Arena owns this
};

// Usage in evaluator
void RealEvaluator::visit(const FunctionCall& node) {
    const UserFunction* userFunc = functions_.lookupUser(node.name());
    if (userFunc) {
        // Evaluate arguments
        std::vector<double> argValues;
        for (auto* arg : node.arguments()) {
            arg->accept(*this);
            argValues.push_back(pop());
        }

        // Create local scope with parameters bound to arguments
        SymbolTable localScope(const_cast<SymbolTable*>(&symbols_));
        for (size_t i = 0; i < userFunc->parameters().size(); ++i) {
            // Create NumberLiteral for argument value
            auto* argExpr = arena_->make<NumberLiteral>(argValues[i]);
            localScope.define(userFunc->parameters()[i], argExpr);
        }

        // Evaluate body in local scope
        RealEvaluator localEval(localScope, functions_, circuit_, arena_);
        stack_.push(localEval.evaluate(*userFunc->body()));
        return;
    }

    // Fall back to built-in
    // ...
}
```

---

## Circuit Interface

### Abstract Interface

```cpp
class CircuitInterface {
public:
    virtual double getNodeVoltageReal(const std::string& node) const = 0;
    virtual double getDeviceCurrentReal(const std::string& device) const = 0;
    virtual double getDifferentialVoltageReal(const std::string& n1, const std::string& n2) const = 0;

    virtual std::complex<double> getNodeVoltageComplex(const std::string& node) const = 0;
    virtual std::complex<double> getDeviceCurrentComplex(const std::string& device) const = 0;
    virtual std::complex<double> getDifferentialVoltageComplex(const std::string& n1, const std::string& n2) const = 0;
};
```

### Mock Implementation for Testing

```cpp
class MockCircuitInterface : public CircuitInterface {
    std::unordered_map<std::string, double> nodeVoltagesReal_;
    std::unordered_map<std::string, double> deviceCurrentsReal_;

public:
    void setNodeVoltage(const std::string& node, double voltage) {
        nodeVoltagesReal_[node] = voltage;
    }

    double getNodeVoltageReal(const std::string& node) const override {
        auto it = nodeVoltagesReal_.find(node);
        return (it != nodeVoltagesReal_.end()) ? it->second : 0.0;
    }
    // ...
};
```

### Null Implementation

```cpp
class NullCircuitInterface : public CircuitInterface {
    double getNodeVoltageReal(const std::string&) const override { return 0.0; }
    // All methods return zero - used when no circuit is available
};
```

---

## Symbolic Differentiation

### DiffTarget: What to Differentiate With Respect To

```cpp
class DiffTarget {
    struct Parameter { std::string name; };
    struct NodeVoltage { std::string node; };
    struct DeviceCurrent { std::string device; };

    std::variant<Parameter, NodeVoltage, DeviceCurrent> target_;

public:
    static DiffTarget parameter(const std::string& name);
    static DiffTarget nodeVoltage(const std::string& node);
    static DiffTarget deviceCurrent(const std::string& device);

    bool isParameter() const;
    bool isNodeVoltage() const;
    bool isDeviceCurrent() const;
};
```

### Differentiation Rules

```cpp
class Differentiator : public ConstExprVisitor {
    ExprArena& arena_;
    DiffTarget target_;
    ExprNode* result_;
    const SymbolTable* symbols_ = nullptr;  // For chain rule through parameters
    std::unordered_set<std::string> visiting_;  // Cycle detection

    ExprNode* zero() { return arena_.make<NumberLiteral>(0.0); }
    ExprNode* one() { return arena_.make<NumberLiteral>(1.0); }

    // d/dx(constant) = 0
    void visit(const NumberLiteral&) override { result_ = zero(); }

    // d/dx(x) = 1, d/dx(y) = dy/dx (chain rule if symbols_ provided)
    void visit(const Identifier& node) override {
        if (target_.isParameter() && node.name() == target_.name()) {
            result_ = one();
            return;
        }

        // Chain rule through parameter dependencies
        if (symbols_) {
            // Cycle detection
            if (visiting_.count(node.name()) > 0) {
                throw std::runtime_error("Circular dependency: " + node.name());
            }

            const SymbolEntry* entry = symbols_->lookup(node.name());
            if (entry && entry->expression) {
                visiting_.insert(node.name());
                result_ = differentiate(*entry->expression);  // Recursive
                visiting_.erase(node.name());
                return;
            }
        }

        result_ = zero();
    }

    // d/dx(V(n)) = 1 if differentiating w.r.t. V(n), else 0
    void visit(const CircuitNodeRef& node) override {
        if (target_.isNodeVoltage() && node.node1() == target_.name()) {
            result_ = one();
        } else {
            result_ = zero();
        }
    }

    // Product rule: d/dx(f*g) = f'*g + f*g'
    void visit(const BinaryOp& node) override {
        if (node.opType() == BinaryOpType::Multiply) {
            ExprNode* f = cloneNode(*node.left());
            ExprNode* g = cloneNode(*node.right());
            ExprNode* df = differentiate(*node.left());
            ExprNode* dg = differentiate(*node.right());

            result_ = arena_.make<BinaryOp>(BinaryOpType::Add,
                arena_.make<BinaryOp>(BinaryOpType::Multiply, df, g),
                arena_.make<BinaryOp>(BinaryOpType::Multiply, f, dg));
        }
        // Quotient rule, chain rule for power, etc.
    }

    // Chain rule: d/dx(f(g(x))) = f'(g(x)) * g'(x)
    void visit(const FunctionCall& node) override {
        ExprNode* arg = cloneNode(*node.arguments()[0]);
        ExprNode* argDeriv = differentiate(*node.arguments()[0]);

        ExprNode* funcDeriv = nullptr;
        if (node.name() == "sin") {
            funcDeriv = arena_.make<FunctionCall>("cos", {arg});
        } else if (node.name() == "cos") {
            funcDeriv = arena_.make<UnaryOp>(UnaryOpType::Negate,
                arena_.make<FunctionCall>("sin", {arg}));
        }
        // ... other functions

        result_ = arena_.make<BinaryOp>(BinaryOpType::Multiply, funcDeriv, argDeriv);
    }
};
```

---

## Expression Simplification

### Simplification Rules

```cpp
class Simplifier : public ConstExprVisitor {
    void visit(const BinaryOp& node) override {
        ExprNode* left = simplify(*node.left());
        ExprNode* right = simplify(*node.right());

        // Constant folding
        if (left->type() == NodeType::NumberLiteral &&
            right->type() == NodeType::NumberLiteral) {
            double l = static_cast<NumberLiteral*>(left)->real();
            double r = static_cast<NumberLiteral*>(right)->real();
            result_ = arena_.make<NumberLiteral>(l + r);  // for Add
            return;
        }

        // Algebraic identities
        if (node.opType() == BinaryOpType::Add) {
            // x + 0 = x
            if (isZero(right)) { result_ = left; return; }
            // 0 + x = x
            if (isZero(left)) { result_ = right; return; }
        }

        if (node.opType() == BinaryOpType::Multiply) {
            // x * 0 = 0
            if (isZero(left) || isZero(right)) { result_ = zero(); return; }
            // x * 1 = x
            if (isOne(right)) { result_ = left; return; }
            // 1 * x = x
            if (isOne(left)) { result_ = right; return; }
        }

        // No simplification possible
        result_ = arena_.make<BinaryOp>(node.opType(), left, right);
    }
};
```

### Simplification to Fixpoint

```cpp
ExprNode* Simplifier::simplifyToFixpoint(const ExprNode& expr, int maxIterations) {
    ExprNode* current = simplify(expr);

    for (int i = 0; i < maxIterations; ++i) {
        ExprNode* next = simplify(*current);
        if (current->equals(*next)) {
            return current;  // Reached fixpoint
        }
        current = next;
    }

    return current;
}
```

---

## Parser and Lexer

### Lexer: Tokenization

```cpp
enum class TokenType {
    Number, Identifier, String,
    Plus, Minus, Star, Slash, Percent, Power,
    LParen, RParen, LBracket, RBracket,
    Comma, Question, Colon,
    Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual,
    And, Or, Not,
    EndOfInput, Error
};

class Lexer {
    std::string input_;
    size_t pos_;

    Token nextToken() {
        skipWhitespace();
        if (pos_ >= input_.size()) return Token(TokenType::EndOfInput);

        char c = input_[pos_];

        // Numbers with engineering notation
        if (std::isdigit(c) || c == '.') {
            return scanNumber();
        }

        // Identifiers
        if (std::isalpha(c) || c == '_') {
            return scanIdentifier();
        }

        // Operators
        switch (c) {
            case '+': pos_++; return Token(TokenType::Plus);
            case '-': pos_++; return Token(TokenType::Minus);
            // ...
        }
    }

    Token scanNumber() {
        // Parse mantissa
        std::string num;
        while (pos_ < input_.size() &&
               (std::isdigit(input_[pos_]) || input_[pos_] == '.')) {
            num += input_[pos_++];
        }

        // Parse exponent
        if (pos_ < input_.size() &&
            (input_[pos_] == 'e' || input_[pos_] == 'E')) {
            // ...
        }

        // Engineering suffix
        double multiplier = parseEngineeringSuffix();

        double value = std::stod(num) * multiplier;
        return Token(TokenType::Number, value);
    }

    double parseEngineeringSuffix() {
        if (pos_ >= input_.size()) return 1.0;

        char c = input_[pos_];
        switch (c) {
            case 'f': pos_++; return 1e-15;
            case 'p': pos_++; return 1e-12;
            case 'n': pos_++; return 1e-9;
            case 'u': pos_++; return 1e-6;
            case 'm': pos_++; return 1e-3;
            case 'k': pos_++; return 1e3;
            case 'G': pos_++; return 1e9;
            case 'T': pos_++; return 1e12;
            // MEG handled specially
            default: return 1.0;
        }
    }
};
```

### Parser: Recursive Descent

```cpp
class Parser {
    Lexer lexer_;
    ExprArena& arena_;
    Token current_;

    // expression → ternary
    ExprNode* expression() { return ternary(); }

    // ternary → logicalOr ('?' expression ':' expression)?
    ExprNode* ternary() {
        ExprNode* expr = logicalOr();
        if (match(TokenType::Question)) {
            ExprNode* trueExpr = expression();
            consume(TokenType::Colon);
            ExprNode* falseExpr = expression();
            expr = arena_.make<TernaryConditional>(expr, trueExpr, falseExpr);
        }
        return expr;
    }

    // Precedence climbing for binary operators
    // additive → multiplicative (('+' | '-') multiplicative)*
    ExprNode* additive() {
        ExprNode* expr = multiplicative();
        while (true) {
            if (match(TokenType::Plus)) {
                ExprNode* right = multiplicative();
                expr = arena_.make<BinaryOp>(BinaryOpType::Add, expr, right);
            } else if (match(TokenType::Minus)) {
                ExprNode* right = multiplicative();
                expr = arena_.make<BinaryOp>(BinaryOpType::Subtract, expr, right);
            } else {
                break;
            }
        }
        return expr;
    }

    // primary → NUMBER | IDENTIFIER | '(' expression ')' | function_call | ...
    ExprNode* primary() {
        if (check(TokenType::Number)) {
            double val = current_.numberValue();
            advance();
            return arena_.make<NumberLiteral>(val);
        }

        if (check(TokenType::Identifier)) {
            std::string name = current_.text;
            advance();

            // Check for V(node) or I(device)
            if ((name == "V" || name == "v") && check(TokenType::LParen)) {
                return circuitRef("V");
            }
            if ((name == "I" || name == "i") && check(TokenType::LParen)) {
                return circuitRef("I");
            }

            // Check for function call
            if (check(TokenType::LParen)) {
                return functionCall(name);
            }

            return arena_.make<Identifier>(name);
        }

        if (match(TokenType::LParen)) {
            ExprNode* expr = expression();

            // Complex literal: (real, imag)
            if (match(TokenType::Comma)) {
                double real = static_cast<NumberLiteral*>(expr)->real();
                ExprNode* imagExpr = expression();
                double imag = static_cast<NumberLiteral*>(imagExpr)->real();
                consume(TokenType::RParen);
                return arena_.make<NumberLiteral>(real, imag);
            }

            consume(TokenType::RParen);
            return expr;
        }

        error("Expected expression");
        return arena_.make<NumberLiteral>(0.0);
    }
};
```

---

## Expression Engine Facade

### Unified Interface

```cpp
class ExpressionEngine {
    ExprArena arena_;
    SymbolTable symbols_;
    FunctionRegistry functions_;
    EvaluationOrderManager orderManager_;
    NullCircuitInterface nullCircuit_;

public:
    ExprArena& arena() { return arena_; }

    // Parameter management
    void defineParameter(const std::string& name, ExprNode* expression, bool isGlobal = false);
    void setParameterOverride(const std::string& name, ExprNode* expression);
    void clearParameterOverride(const std::string& name);

    // Function management
    void defineFunction(const std::string& name,
                        const std::vector<std::string>& parameters,
                        ExprNode* body);

    // Dependency analysis
    void buildEvaluationOrder();
    bool hasCyclicDependencies() const;

    // Evaluation
    double evaluateReal(const ExprNode& expr, const CircuitInterface& circuit) const;
    std::complex<double> evaluateComplex(const ExprNode& expr, const CircuitInterface& circuit) const;
    void evaluateAllParameters(const CircuitInterface& circuit);

    // Symbolic operations
    ExprNode* simplify(const ExprNode& expr);
    ExprNode* differentiate(const ExprNode& expr, const DiffTarget& target);

    // Scoping
    std::unique_ptr<SymbolTable> createSubcircuitScope();

    // Parameter flagging
    void set_parameter_flag(const std::string& name, const std::string& flag);
    void clear_parameter_flag(const std::string& name, const std::string& flag);
    bool has_parameter_flag(const std::string& name, const std::string& flag) const;
    std::vector<ExprNode*> get_parameters_with_flag(const std::string& flag) const;

    // Affected parameter queries (returns ExprNode* for hierarchical scope safety)
    std::set<ExprNode*> get_affected_parameters(const ExprNode& expr) const;
    std::set<ExprNode*> get_affected_parameters_by_flag(const std::string& flag) const;
};
```

### Typical Usage

```cpp
ExpressionEngine engine;
ExprArena& arena = engine.arena();

// Define parameters
engine.defineParameter("vdd", arena.make<NumberLiteral>(1.8));
engine.defineParameter("vth", arena.make<NumberLiteral>(0.4));
engine.defineParameter("vov", arena.make<BinaryOp>(BinaryOpType::Subtract,
    arena.make<Identifier>("vdd"),
    arena.make<Identifier>("vth")));

// Build evaluation order (detects cycles)
engine.buildEvaluationOrder();
if (engine.hasCyclicDependencies()) {
    // Handle error
}

// Evaluate with circuit values
MockCircuitInterface circuit;
circuit.setNodeVoltage("out", 0.9);

ExprNode* expr = parseExpression("vov * V(out)", arena);
double result = engine.evaluateReal(*expr, circuit);

// Differentiate for sensitivity
ExprNode* deriv = engine.differentiate(*expr, DiffTarget::parameter("vdd"));
double sensitivity = engine.evaluateReal(*deriv, circuit);

// Flag parameters for optimization
engine.set_parameter_flag("vdd", "optimize");
engine.set_parameter_flag("vth", "optimize");

// Find all parameters affected by optimization targets (returns ExprNode* set)
auto affected = engine.get_affected_parameters_by_flag("optimize");
// affected contains expression nodes for vdd, vth, vov, and any dependent parameters
```

---

## Design Decisions and Trade-offs

### 1. Arena Allocation vs. Smart Pointers

**Decision:** Use arena allocation with raw pointers.

**Trade-offs:**
- **Pro:** Zero reference counting overhead, cache-friendly memory layout
- **Pro:** Simple ownership model - arena owns everything
- **Con:** Nodes can't outlive the arena
- **Con:** No fine-grained deallocation (arena is all-or-nothing)

**Rationale:** Expression trees are typically built, evaluated, and discarded together. Arena allocation fits this pattern perfectly.

### 2. Dual Evaluators vs. Single Polymorphic Evaluator

**Decision:** Separate `RealEvaluator` and `ComplexEvaluator`.

**Trade-offs:**
- **Pro:** Real path has zero complex number overhead
- **Pro:** Compiler can optimize each path independently
- **Con:** Code duplication between evaluators
- **Con:** Must choose correct evaluator at call site

**Rationale:** Performance-critical path. Most expressions are real-only, so avoiding complex overhead is significant.

### 3. Visitor Pattern vs. Node Methods

**Decision:** Use visitor pattern for operations.

**Trade-offs:**
- **Pro:** Easy to add new operations without modifying node classes
- **Pro:** Operations can have shared state (e.g., evaluation stack)
- **Con:** Adding new node types requires updating all visitors
- **Con:** More boilerplate code

**Rationale:** Operations (evaluation, simplification, differentiation) change more frequently than node types.

### 4. NodeType Enum vs. RTTI

**Decision:** Use `NodeType` enum for type identification.

**Trade-offs:**
- **Pro:** Faster than `dynamic_cast` or `typeid`
- **Pro:** Works with compilers that have RTTI disabled
- **Con:** Must keep enum in sync with class hierarchy
- **Con:** No inheritance-aware type checking

**Rationale:** Type checking happens frequently in simplification and evaluation. The performance benefit justifies the maintenance overhead.

### 5. Stack-Based Evaluation vs. Return Values

**Decision:** Evaluators use internal value stack.

**Trade-offs:**
- **Pro:** Avoids temporary allocations for return values
- **Pro:** Natural fit for expression tree structure
- **Con:** State management (must clear stack between evaluations)
- **Con:** Stack underflow is a potential bug source

**Rationale:** Expression evaluation is performance-critical. Stack-based approach minimizes overhead.

---

## Conclusion

The spice_expr module demonstrates several key software engineering principles:

1. **Separation of Concerns:** AST structure, traversal, and operations are cleanly separated
2. **Performance Optimization:** Arena allocation and dual evaluation paths minimize overhead
3. **Extensibility:** Visitor pattern allows adding new operations without modifying core classes
4. **SPICE Compatibility:** Supports hierarchical scoping, circuit references, and engineering notation

The module is designed to be embedded in a larger SPICE simulator, providing efficient expression handling for parameter evaluation, sensitivity analysis, and symbolic manipulation.
