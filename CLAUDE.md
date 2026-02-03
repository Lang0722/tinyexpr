# CLAUDE.md - AI Assistant Guide for spice_expr

## Project Overview

This is a high-performance expression evaluation module for HSPICE-like netlists in C++17. It provides parsing, evaluation, symbolic manipulation, and differentiation of mathematical expressions with support for circuit references (V(node), I(device)).

## Build Commands

```bash
# Configure and build
cd build
cmake ..
cmake --build .

# Run all tests
./spice_expr_tests

# Run specific test suite
./spice_expr_tests --gtest_filter="ParserTest.*"

# Run single test
./spice_expr_tests --gtest_filter="IntegrationTest.ParameterOverride"
```

## Project Structure

```
tinyexpr/
├── include/spice_expr/
│   ├── ast/           # AST node definitions
│   │   ├── arena.h        # Memory arena for node allocation
│   │   ├── expr_node.h    # Base ExprNode class
│   │   ├── literals.h     # NumberLiteral, StringLiteral, ArrayLiteral
│   │   ├── references.h   # Identifier, CircuitNodeRef, CircuitCurrentRef
│   │   ├── operations.h   # BinaryOp, UnaryOp
│   │   └── functions.h    # FunctionCall, ArrayIndex, TernaryConditional
│   ├── visitor/       # Visitor pattern implementations
│   │   ├── visitor.h          # Base visitor interfaces
│   │   ├── evaluator.h        # RealEvaluator, ComplexEvaluator
│   │   ├── printer.h          # Expression pretty-printing
│   │   ├── simplifier.h       # Algebraic simplification
│   │   ├── differentiator.h   # Symbolic differentiation
│   │   └── dependency_extractor.h
│   ├── symbol/        # Symbol table with hierarchical scoping
│   ├── function/      # Built-in and user-defined functions
│   ├── circuit/       # Circuit interface (V/I references)
│   ├── analysis/      # Dependency graph, cycle detection
│   ├── parser/        # Lexer and recursive descent parser
│   ├── array/         # Array value types
│   ├── engine/        # ExpressionEngine facade
│   └── spice_expr.h   # Main include header
├── src/               # Implementation files (.cc)
└── tests/             # Google Test files
```

## Key Architecture Concepts

### Memory Management
- **Arena Allocation**: All AST nodes are allocated via `ExprArena`
- **No shared_ptr**: Raw `ExprNode*` pointers everywhere
- Nodes are valid for the lifetime of the arena/engine
- `clone(ExprArena&)` creates deep copies in a target arena

### CMake Library Structure
- **spice_expr_ast**: Core AST node definitions (shared by frontend and backend)
- **spice_expr_parser**: Frontend (lexer and parser)
- **spice_expr_core**: Backend (evaluator, simplifier, differentiator, etc.)
- **spice_expr**: Combined interface library (links parser + core)

### Visitor Pattern
- `ExprVisitor` for non-const operations
- `ConstExprVisitor` for read-only traversal
- Each visitor maintains internal state (e.g., value stack in evaluators)

### Dual Evaluation Paths
- `RealEvaluator`: Fast path using `double` only
- `ComplexEvaluator`: Handles `std::complex<double>`
- `requires_complex_evaluation()` detects which path to use

### Symbol Table Scoping
- `ScopingMode::Local` - search current scope, then parent
- `ScopingMode::Global` - search root scope first (HSPICE PARHIER=GLOBAL)
- Override mechanism for parameter sweeps
- Child scopes for subcircuits

## Common Patterns

### Creating Expressions
```cpp
ExprArena arena;
auto* expr = arena.make<BinaryOp>(BinaryOpType::Add,
    arena.make<NumberLiteral>(1.0),
    arena.make<Identifier>("x"));
```

### Evaluating Expressions
```cpp
ExpressionEngine engine;
engine.define_parameter("x", arena.make<NumberLiteral>(2.0));
double result = engine.evaluate_real(*expr);
```

### Parsing Expressions
```cpp
ExprArena arena;
ExprNode* expr = parse_expression("sin(x) + 2*y", arena);
```

## Testing Conventions

- Test files mirror source structure: `tests/<component>_tests.cc`
- Use `TEST_F` with fixtures for shared setup
- Mock circuit interface: `MockCircuitInterface`
- Null circuit for pure parameter evaluation: `NullCircuitInterface`

## Important Implementation Details

### Parser
- Recursive descent with standard precedence
- Engineering notation: f, p, n, u, m, k, MEG, G, T
- Complex literals: `(real, imag)`
- Case-insensitive function names

### Differentiator
- Supports differentiation w.r.t. parameters, V(node), I(device)
- `DiffTarget` variant type specifies the differentiation variable
- Chain rule applied automatically for function composition
- **Chain rule through parameter dependencies**: When a `SymbolTable*` is provided,
  the differentiator resolves parameter definitions (e.g., `b = 2*a` → `db/da = 2`)
- Cycle detection prevents infinite recursion in circular dependencies
- Result is simplified to fixpoint

### Symbol Table Overrides
- `set_override()` temporarily replaces expression
- `clear_override()` restores original
- Original expressions tracked in `original_expressions_` map

### Parameter Flagging
- Parameters can be tagged with arbitrary string flags (e.g., `optimize`, `sweep`)
- `set_flag()`, `clear_flag()`, `has_flag()`, `get_flags()` for flag management
- `get_parameters_with_flag()` returns `std::vector<ExprNode*>` - the expression nodes directly
- Useful for marking parameters for optimization, sensitivity analysis, or sweeps

### Affected Parameter Queries
- `get_transitive_dependents()` in `DependencyGraph` finds all parameters that depend on a given parameter
- `get_affected_parameters(const ExprNode& expr)` takes an expression and returns `std::set<ExprNode*>` of all affected parameters
- `get_affected_parameters_by_flag()` returns `std::set<ExprNode*>` - flagged parameters plus all their transitive dependents
- Returns expression nodes directly to avoid ambiguity with hierarchical scoping

## Debugging Tips

1. **Expression printing**: Use `Printer` with `PrintFormat::Infix` to see expression structure
2. **Dependency issues**: Check `DependencyGraph::has_cycle()` and `find_cycle()`
3. **Evaluation errors**: `EvaluationError` includes descriptive messages
4. **Parser errors**: `ParseError` includes line/column information

## Files Most Likely to Need Changes

- `src/evaluator.cc` - Add new built-in behavior
- `src/function_registry.cc` - Add new built-in functions
- `src/differentiator.cc` - Add derivative rules for new functions
- `src/parser.cc` - Extend syntax support
- `include/spice_expr/ast/*.h` - Add new node types

## Coding Conventions

### Naming Style
- **Classes**: PascalCase (`ExprNode`, `SymbolTable`)
- **Methods**: snake_case (`get_node()`, `evaluate_real()`)
- **Member variables**: snake_case with trailing underscore (`arena_`, `symbols_`)
- **Single-word methods**: No transformation needed (`name()`, `type()`, `value()`, `clone()`, `hash()`)

### Code Formatting
- Use clang-format with Google style base
- Run `clang-format -i <file>` before committing
- Configuration in `.clang-format` at project root
- Column limit: 100 characters
- Indent: 2 spaces
