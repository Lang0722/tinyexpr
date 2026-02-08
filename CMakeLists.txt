cmake_minimum_required(VERSION 3.14)
cmake_policy(SET CMP0077 NEW)
project(spice_expr VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Export compile commands for IDE support
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# tinytensor for numpy-like array operations (lightweight, header-only C++20)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/../tinytensor tinytensor)

# AST library (shared dependency)
set(SPICE_EXPR_AST_SOURCES
    src/arena.cc
    src/expr_node.cc
    src/literals.cc
    src/references.cc
    src/operations.cc
    src/functions.cc
)

add_library(spice_expr_ast STATIC ${SPICE_EXPR_AST_SOURCES})
target_include_directories(spice_expr_ast PUBLIC include)
target_compile_options(spice_expr_ast PRIVATE -Wall -Wextra -Wpedantic)

# Parser library (frontend)
set(SPICE_EXPR_PARSER_SOURCES
    src/lexer.cc
    src/parser.cc
)

add_library(spice_expr_parser STATIC ${SPICE_EXPR_PARSER_SOURCES})
target_include_directories(spice_expr_parser PUBLIC include)
target_link_libraries(spice_expr_parser PUBLIC spice_expr_ast)
target_compile_options(spice_expr_parser PRIVATE -Wall -Wextra -Wpedantic)

# Core library (backend)
set(SPICE_EXPR_CORE_SOURCES
    src/evaluator.cc
    src/printer.cc
    src/simplifier.cc
    src/differentiator.cc
    src/dependency_extractor.cc
    src/dependency_graph.cc
    src/dependency_printer.cc
    src/evaluation_order.cc
    src/symbol_table.cc
    src/function_registry.cc
    src/user_function.cc
    src/circuit_interface.cc
    src/array_value.cc
    src/xtensor.cc
    src/tensor_evaluator.cc
    src/expression_engine.cc
)

add_library(spice_expr_core STATIC ${SPICE_EXPR_CORE_SOURCES})
target_include_directories(spice_expr_core PUBLIC include)
target_link_libraries(spice_expr_core PUBLIC spice_expr_ast tinytensor::tinytensor)
target_compile_options(spice_expr_core PRIVATE -Wall -Wextra -Wpedantic)

# Combined library (backward compatible)
add_library(spice_expr INTERFACE)
target_link_libraries(spice_expr INTERFACE spice_expr_parser spice_expr_core)

# Testing with Google Test
include(FetchContent)
FetchContent_Declare(
    googletest
    GIT_REPOSITORY https://github.com/google/googletest.git
    GIT_TAG v1.14.0
)
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

enable_testing()

set(TEST_SOURCES
    tests/ast_tests.cc
    tests/evaluator_tests.cc
    tests/symbol_table_tests.cc
    tests/dependency_tests.cc
    tests/dependency_printer_tests.cc
    tests/function_tests.cc
    tests/array_tests.cc
    tests/xtensor_tests.cc
    tests/tensor_evaluator_tests.cc
    tests/parser_tests.cc
    tests/differentiator_tests.cc
    tests/simplifier_tests.cc
    tests/integration_tests.cc
    tests/compact_model_tests.cc
    tests/expression_corpus_tests.cc
    tests/stress_tests.cc
    tests/advanced_evaluation_tests.cc
    tests/exprtk_corpus_tests.cc
    tests/exprtk_functional_tests.cc
    tests/exprtk_ext_tests_part1.cc
)

add_executable(spice_expr_tests ${TEST_SOURCES})
target_link_libraries(spice_expr_tests PRIVATE spice_expr GTest::gtest_main)
target_compile_options(spice_expr_tests PRIVATE -Wall -Wextra -Wpedantic)

include(GoogleTest)
gtest_discover_tests(spice_expr_tests)

# Optional benchmark support
option(SPICE_EXPR_BUILD_BENCHMARKS "Build benchmark tests" OFF)

if(SPICE_EXPR_BUILD_BENCHMARKS)
  FetchContent_Declare(
      benchmark
      GIT_REPOSITORY https://github.com/google/benchmark.git
      GIT_TAG v1.8.3
  )
  set(BENCHMARK_ENABLE_TESTING OFF CACHE BOOL "" FORCE)
  set(BENCHMARK_ENABLE_INSTALL OFF CACHE BOOL "" FORCE)
  FetchContent_MakeAvailable(benchmark)

  add_executable(spice_expr_benchmarks tests/benchmark_tests.cc)
  target_link_libraries(spice_expr_benchmarks
      PRIVATE spice_expr benchmark::benchmark benchmark::benchmark_main)
  target_compile_options(spice_expr_benchmarks PRIVATE -Wall -Wextra -Wpedantic)
endif()
