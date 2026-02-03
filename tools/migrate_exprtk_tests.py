#!/usr/bin/env python3
"""
Migrate ExprTK tests to spice_expr format.

This script parses ExprTK test files and converts them to spice_expr test format,
applying necessary syntax transformations and filtering incompatible tests.
"""

import argparse
import logging
import os
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(levelname)s: %(message)s'
)
logger = logging.getLogger(__name__)


@dataclass
class TestCase:
    """Represents a single test case with expression and expected value."""
    expr: str
    expected: float
    line_num: int = 0
    source_file: str = ""


@dataclass
class FilterResult:
    """Result of filtering a test case."""
    should_skip: bool
    reason: str = ""


@dataclass
class ConversionStats:
    """Statistics about the conversion process."""
    total: int = 0
    converted: int = 0
    filtered: int = 0
    filter_reasons: dict = None

    def __post_init__(self):
        if self.filter_reasons is None:
            self.filter_reasons = {}


# =============================================================================
# Parsers
# =============================================================================

def parse_test_t_file(filepath: Path) -> list[TestCase]:
    """
    Parse test_t{"expr", value} pairs from exprtk_test.cpp.

    Returns a list of TestCase objects.
    """
    tests = []
    content = filepath.read_text()

    # Match test_t("expr", value) or test_t("expr",value)
    # Handle multi-line expressions and various numeric formats
    pattern = r'test_t\s*\(\s*"((?:[^"\\]|\\.)*)"\s*,\s*([^)]+)\)'

    for match in re.finditer(pattern, content):
        expr = match.group(1)
        value_str = match.group(2).strip()

        # Unescape the expression
        expr = expr.replace('\\"', '"').replace('\\\\', '\\')
        expr = expr.replace('\\t', '\t').replace('\\n', '\n').replace('\\r', '\r')

        # Parse the expected value
        try:
            expected = parse_numeric_value(value_str)
            # Find line number
            line_num = content[:match.start()].count('\n') + 1
            tests.append(TestCase(expr, expected, line_num, str(filepath)))
        except ValueError as e:
            logger.debug(f"Skipping test with unparseable value '{value_str}': {e}")

    return tests


def parse_equal_file(filepath: Path) -> list[TestCase]:
    """
    Parse equal(expr, value) lines from .txt files.

    Each line has format: equal(expression, expected_value)
    Handles nested parentheses in expressions.
    """
    tests = []

    with open(filepath, 'r') as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            if not line or not line.startswith('equal('):
                continue

            # Find the matching closing paren for the equal() call
            test = parse_equal_line(line, line_num, str(filepath))
            if test:
                tests.append(test)

    return tests


def parse_equal_line(line: str, line_num: int, source_file: str) -> Optional[TestCase]:
    """
    Parse a single equal(arg1, arg2) line.

    The functional test files use format: equal(expected_value, expression)
    where expected_value is a number (possibly with leading zeros and parentheses).

    Handles nested parentheses properly.
    """
    if not line.startswith('equal('):
        return None

    # Remove 'equal(' prefix
    content = line[6:]

    # Find the FIRST comma that separates the two arguments at depth 0
    # In functional tests, format is: equal(value, expr)
    depth = 0
    first_comma_pos = -1

    for i, char in enumerate(content):
        if char in '([{':
            depth += 1
        elif char in ')]}':
            depth -= 1
        elif char == ',' and depth == 0:
            first_comma_pos = i
            break

    if first_comma_pos == -1:
        logger.debug(f"No comma found in equal() at line {line_num}")
        return None

    first_arg = content[:first_comma_pos].strip()
    second_arg = content[first_comma_pos + 1:].strip()

    # Remove trailing ) from second arg (closing the equal() call)
    if second_arg.endswith(')'):
        second_arg = second_arg[:-1].strip()

    # Remove surrounding parentheses from both args if present
    first_arg = strip_outer_parens(first_arg)
    second_arg = strip_outer_parens(second_arg)

    # Try to parse first_arg as number (functional test format: equal(value, expr))
    try:
        expected = parse_numeric_value(first_arg)
        return TestCase(second_arg, expected, line_num, source_file)
    except ValueError:
        pass

    # Try to parse second_arg as number (alternative format: equal(expr, value))
    try:
        expected = parse_numeric_value(second_arg)
        return TestCase(first_arg, expected, line_num, source_file)
    except ValueError as e:
        logger.debug(f"Cannot parse either argument as value at line {line_num}: {e}")
        return None


def strip_outer_parens(s: str) -> str:
    """Strip outermost matching parentheses from a string."""
    s = s.strip()
    while s.startswith('(') and s.endswith(')'):
        # Verify the parens actually match (not just both present)
        depth = 0
        matched = True
        for i, char in enumerate(s):
            if char == '(':
                depth += 1
            elif char == ')':
                depth -= 1
                if depth == 0 and i < len(s) - 1:
                    # Closing paren found before end - outer parens don't match
                    matched = False
                    break
        if matched:
            s = s[1:-1].strip()
        else:
            break
    return s


def parse_numeric_value(value_str: str) -> float:
    """
    Parse a numeric value string to float.

    Handles:
    - Regular numbers: 1.0, -2.5, +3.14
    - Scientific notation: 1.1e+1, 2.2E-3
    - Leading zeros: 00000000000000000001.0037...
    - C++ constants (will be converted)
    """
    value_str = value_str.strip()

    # Remove trailing 'f' or 'F' for float literals
    if value_str.endswith('f') or value_str.endswith('F'):
        value_str = value_str[:-1]

    # Remove leading + sign
    if value_str.startswith('+'):
        value_str = value_str[1:]

    # Handle special string values that need conversion
    if value_str.lower() in ('true', '1.0'):
        return 1.0
    if value_str.lower() in ('false', '0.0'):
        return 0.0

    return float(value_str)


# =============================================================================
# Filters
# =============================================================================

# Patterns that indicate unsupported features
FILTER_PATTERNS = [
    # Scoped blocks
    (r'~\{', "scoped blocks (~{})"),
    (r'\[\*\]', "multi-match blocks ([*])"),

    # Assignment - spice_expr now supports :=, but ExprTK tests use variable state
    # across test cases which our test framework doesn't support
    (r':=', "assignment operator (requires shared state)"),

    # Control flow keywords
    (r'\bswitch\b', "switch statement"),
    (r'\bcase\b', "case statement"),
    (r'\brepeat\b', "repeat loop"),
    (r'\bwhile\b', "while loop"),
    (r'\bfor\b', "for loop"),
    (r'\buntil\b', "until keyword"),
    (r'\bdefault\b', "default keyword"),

    # Unsupported logical operators
    (r'\bnand\b', "nand operator"),
    (r'\bnor\b', "nor operator"),
    (r'\bxor\b', "xor operator"),
    (r'\bxnor\b', "xnor operator"),
    (r'\bmand\b', "mand function"),
    (r'\bmor\b', "mor function"),

    # User-defined function shortcuts
    (r'\$f\d+', "user-defined function shortcut ($f##)"),

    # Variable declarations
    (r'\bvar\s+', "var declaration"),
    (r'\bconst\s+', "const declaration"),

    # Polynomial functions
    (r'\bpoly\d+\b', "polynomial function"),

    # Functions not available in spice_expr
    (r'\bavg\b', "avg function"),
    (r'\bsum\b', "sum function"),
    (r'\bmul\b', "mul function"),
    (r'\bfrac\b', "frac function"),
    (r'\btrunc\b', "trunc function"),
    (r'\binrange\b', "inrange function"),
    (r'\bclamp\b', "clamp function (use limit instead)"),
    (r'\broundn\b', "roundn function"),
    (r'\broot\b', "root function"),
    (r'\bdeg2rad\b', "deg2rad function"),
    (r'\bgrad2deg\b', "grad2deg function"),
    (r'\bequal\b', "equal function"),
    (r'\bnot_equal\b', "not_equal function"),
    (r'\basinh\b', "asinh function"),
    (r'\bacosh\b', "acosh function"),
    (r'\batanh\b', "atanh function"),
    (r'\bncdf\b', "ncdf function"),
    (r'\ball_true\b', "all_true function"),
    (r'\ball_false\b', "all_false function"),
    (r'\bany_true\b', "any_true function"),
    (r'\bany_false\b', "any_false function"),
    (r'\berf\b', "erf function"),
    (r'\berfc\b', "erfc function"),
    (r'\blog1p\b', "log1p function"),
    (r'\bexpm1\b', "expm1 function"),

    # Special constants not defined
    (r'\bepsilon\b', "epsilon constant"),
    (r'\btrue\b', "true constant"),
    (r'\bfalse\b', "false constant"),
    (r'\bpi\b', "pi constant"),
    (r'\binfinity\b', "infinity constant"),

    # Implicit multiplication like 2(3+4)
    (r'\d\s*\(', "implicit multiplication"),

    # Trailing decimal before operator: 2.*3 (ExprTK parses as 2.0*3)
    (r'\d\.\*', "trailing decimal before multiply"),
    (r'\d\.\+', "trailing decimal before add"),

    # Unary minus with power: -2^4 has different precedence in ExprTK vs spice_expr
    # ExprTK: -(2^4) = -16, spice_expr: (-2)**4 = 16
    (r'-\d+\^(?:\d+|[2468])(?:$|[^0-9])', "unary minus power precedence difference"),

    # String literals
    (r"'[^']*'", "string literal"),

    # Square/curly bracket grouping (non-array use)
    (r'\[[^\]]*[+\-*/^<>=&|][^\]]*\]', "square bracket grouping with operators"),
    (r'\{[^\}]*[+\-*/^<>=&|][^\}]*\}', "curly bracket grouping with operators"),
    (r'\[\(', "square bracket with paren [(...)"),
    (r'\)\]', "paren with square bracket ...)]"),

    # Undefined variables (used in functional tests)
    (r'\b[xyzw]\b', "undefined variable (x/y/z/w)"),

    # Semicolon (statement separator)
    (r';', "semicolon (statement separator)"),

    # inf constant
    (r'\binf\b', "inf constant"),
]


def should_filter(expr: str) -> FilterResult:
    """
    Check if an expression should be filtered out.

    Returns FilterResult with should_skip=True and reason if filtered.
    """
    for pattern, reason in FILTER_PATTERNS:
        if re.search(pattern, expr, re.IGNORECASE):
            return FilterResult(True, reason)

    return FilterResult(False)


# =============================================================================
# Transformers
# =============================================================================

def transform_power(expr: str) -> str:
    """Transform ^ to ** for power operator."""
    # Be careful not to replace ^ in contexts where it might not be power
    # In ExprTK, ^ is always power, so we can replace all occurrences
    return expr.replace('^', '**')


def transform_logical_and(expr: str) -> str:
    """Transform 'and' to '&&'."""
    return re.sub(r'\band\b', '&&', expr, flags=re.IGNORECASE)


def transform_logical_or(expr: str) -> str:
    """Transform 'or' to '||'."""
    return re.sub(r'\bor\b', '||', expr, flags=re.IGNORECASE)


def transform_single_ampersand(expr: str) -> str:
    """Transform single '&' to '&&' for logical AND."""
    # Match & not preceded or followed by another &
    return re.sub(r'(?<!&)&(?!&)', '&&', expr)


def transform_single_pipe(expr: str) -> str:
    """Transform single '|' to '||' for logical OR."""
    # Match | not preceded or followed by another |
    return re.sub(r'(?<!\|)\|(?!\|)', '||', expr)


def transform_not_equal(expr: str) -> str:
    """Transform '<>' to '!='."""
    return expr.replace('<>', '!=')


def transform_single_equal(expr: str) -> str:
    """
    Transform single '=' to '==' for comparison.

    Be careful not to affect ':=' or '==' or '<=' or '>=' or '!='.
    """
    # Use negative lookbehind and lookahead
    # Match = not preceded by : < > ! or =, and not followed by =
    return re.sub(r'(?<![:<>=!])=(?!=)', '==', expr)


def transform_not_function(expr: str) -> str:
    """Transform not(x) to !(x)."""
    # Match not( at word boundary
    return re.sub(r'\bnot\s*\(', '!(', expr, flags=re.IGNORECASE)


def transform_if_to_ternary(expr: str) -> str:
    """
    Transform if(condition, true_val, false_val) to (condition)?(true_val):(false_val).

    Handles nested if() calls properly.
    """
    result = []
    i = 0

    while i < len(expr):
        # Look for 'if(' at word boundary
        if expr[i:i+3].lower() == 'if(' and (i == 0 or not expr[i-1].isalnum()):
            # Find the matching arguments
            args = extract_if_arguments(expr, i + 3)
            if args and len(args) == 3:
                condition, true_val, false_val = args
                # Recursively transform nested if() calls
                condition = transform_if_to_ternary(condition)
                true_val = transform_if_to_ternary(true_val)
                false_val = transform_if_to_ternary(false_val)
                result.append(f'({condition})?({true_val}):({false_val})')
                # Skip past the if(...) in the original string
                i = find_matching_paren(expr, i + 2) + 1
                continue
        result.append(expr[i])
        i += 1

    return ''.join(result)


def extract_if_arguments(expr: str, start: int) -> Optional[list[str]]:
    """
    Extract the three arguments from an if() call starting after 'if('.

    Returns [condition, true_val, false_val] or None if parsing fails.
    """
    args = []
    current_arg = []
    depth = 1  # We're already inside the opening paren
    i = start

    while i < len(expr) and depth > 0:
        char = expr[i]

        if char in '([{':
            depth += 1
            current_arg.append(char)
        elif char in ')]}':
            depth -= 1
            if depth == 0:
                # End of if() call
                args.append(''.join(current_arg).strip())
                break
            else:
                current_arg.append(char)
        elif char == ',' and depth == 1:
            # Argument separator at the top level
            args.append(''.join(current_arg).strip())
            current_arg = []
        else:
            current_arg.append(char)

        i += 1

    if len(args) == 3:
        return args
    return None


def find_matching_paren(expr: str, start: int) -> int:
    """Find the position of the closing paren matching the one at start."""
    depth = 0
    for i in range(start, len(expr)):
        if expr[i] in '([{':
            depth += 1
        elif expr[i] in ')]}':
            depth -= 1
            if depth == 0:
                return i
    return len(expr) - 1


def transform_all(expr: str) -> str:
    """Apply all transformations to an expression."""
    expr = transform_not_equal(expr)  # <> to !=
    expr = transform_single_equal(expr)  # = to == (before logical ops)
    expr = transform_logical_and(expr)  # and to &&
    expr = transform_logical_or(expr)  # or to ||
    expr = transform_single_ampersand(expr)  # & to &&
    expr = transform_single_pipe(expr)  # | to ||
    expr = transform_not_function(expr)  # not() to !()
    expr = transform_if_to_ternary(expr)  # if(c,t,f) to ternary
    expr = transform_power(expr)  # ^ to **
    return expr


# =============================================================================
# Test Generation
# =============================================================================

def escape_cpp_string(s: str) -> str:
    """Escape a string for use in C++ source code."""
    return s.replace('\\', '\\\\').replace('"', '\\"')


def format_double(value: float) -> str:
    """Format a double value for C++ source code."""
    if value == int(value) and abs(value) < 1e15:
        return f"{int(value)}.0"

    # Use repr for best precision, then adjust format
    s = repr(value)

    # Ensure it has a decimal point or exponent for C++
    if '.' not in s and 'e' not in s.lower():
        s += '.0'

    return s


def generate_test_file(tests: list[TestCase], output_path: Path,
                       category_name: str, suite_prefix: str) -> int:
    """
    Generate a C++ test file from test cases.

    Returns the number of tests written.
    """
    # Group tests into reasonable-sized categories
    TESTS_PER_CATEGORY = 500

    content = []
    content.append('#include <gtest/gtest.h>')
    content.append('')
    content.append('#include <cmath>')
    content.append('#include <string>')
    content.append('#include <vector>')
    content.append('')
    content.append('#include "spice_expr/spice_expr.h"')
    content.append('')
    content.append('using namespace spice_expr;')
    content.append('')
    content.append('// Auto-generated from ExprTK test files')
    content.append('// Syntax adaptations:')
    content.append('// - x^y -> x**y (power operator)')
    content.append('// - and/or -> &&/||')
    content.append('// - if(c,t,f) -> (c)?(t):(f)')
    content.append('// - <> -> !=')
    content.append('// - Tests using ExprTK-specific features are omitted')
    content.append('')
    content.append('struct TestExpr {')
    content.append('  std::string expr;')
    content.append('  double expected;')
    content.append('};')
    content.append('')
    content.append(f'class {category_name}Test : public ::testing::TestWithParam<TestExpr> {{')
    content.append(' protected:')
    content.append('  ExprArena arena;')
    content.append('  SymbolTable symbols;')
    content.append('  FunctionRegistry functions;')
    content.append('  NullCircuitInterface circuit;')
    content.append('')
    content.append('  double evaluate(const std::string& expr_str) {')
    content.append('    ExprNode* expr = parse_expression(expr_str, arena);')
    content.append('    RealEvaluator eval(symbols, functions, circuit);')
    content.append('    return eval.evaluate(*expr);')
    content.append('  }')
    content.append('};')
    content.append('')
    content.append(f'TEST_P({category_name}Test, EvaluatesCorrectly) {{')
    content.append('  const auto& test = GetParam();')
    content.append('  double result = evaluate(test.expr);')
    content.append('  if (std::abs(test.expected) < 1e-10) {')
    content.append('    EXPECT_NEAR(result, test.expected, 1e-10) << "Expr: " << test.expr;')
    content.append('  } else {')
    content.append('    EXPECT_NEAR(result, test.expected, std::abs(test.expected) * 1e-10) << "Expr: " << test.expr;')
    content.append('  }')
    content.append('}')
    content.append('')

    # Split tests into categories
    num_categories = (len(tests) + TESTS_PER_CATEGORY - 1) // TESTS_PER_CATEGORY

    for cat_idx in range(num_categories):
        start_idx = cat_idx * TESTS_PER_CATEGORY
        end_idx = min((cat_idx + 1) * TESTS_PER_CATEGORY, len(tests))
        cat_tests = tests[start_idx:end_idx]

        cat_suffix = f"_{cat_idx}" if num_categories > 1 else ""
        var_name = f"{suite_prefix.lower()}_tests{cat_suffix}"

        content.append(f'static const std::vector<TestExpr> {var_name} = {{')

        for test in cat_tests:
            escaped_expr = escape_cpp_string(test.expr)
            formatted_val = format_double(test.expected)
            content.append(f'    {{"{escaped_expr}", {formatted_val}}},')

        content.append('};')
        content.append('')

        suite_name = f"{suite_prefix}{cat_suffix}"
        content.append(f'INSTANTIATE_TEST_SUITE_P({suite_name}, {category_name}Test,')
        content.append(f'                         ::testing::ValuesIn({var_name}));')
        content.append('')

    output_path.write_text('\n'.join(content))
    return len(tests)


# =============================================================================
# Main
# =============================================================================

def process_file(filepath: Path, stats: ConversionStats) -> list[TestCase]:
    """Process a single input file and return converted test cases."""
    logger.info(f"Processing {filepath.name}...")

    # Parse based on file type
    if filepath.suffix == '.cpp':
        tests = parse_test_t_file(filepath)
    else:
        tests = parse_equal_file(filepath)

    stats.total += len(tests)
    logger.info(f"  Parsed {len(tests)} tests")

    # Filter and transform
    converted = []
    for test in tests:
        filter_result = should_filter(test.expr)
        if filter_result.should_skip:
            stats.filtered += 1
            stats.filter_reasons[filter_result.reason] = \
                stats.filter_reasons.get(filter_result.reason, 0) + 1
            continue

        # Transform the expression
        transformed_expr = transform_all(test.expr)
        converted.append(TestCase(
            transformed_expr,
            test.expected,
            test.line_num,
            test.source_file
        ))

    stats.converted += len(converted)
    logger.info(f"  Converted {len(converted)} tests ({stats.filtered} filtered so far)")

    return converted


def main():
    parser = argparse.ArgumentParser(
        description='Migrate ExprTK tests to spice_expr format'
    )
    parser.add_argument(
        '--input-dir', '-i',
        type=Path,
        default=Path('external/exprtk'),
        help='Directory containing ExprTK test files'
    )
    parser.add_argument(
        '--output-dir', '-o',
        type=Path,
        default=Path('tests'),
        help='Directory for output test files'
    )
    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Enable verbose logging'
    )
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Parse and transform but do not write output files'
    )

    args = parser.parse_args()

    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)

    # Verify input directory
    input_dir = args.input_dir
    if not input_dir.is_absolute():
        input_dir = Path.cwd() / input_dir

    if not input_dir.exists():
        logger.error(f"Input directory not found: {input_dir}")
        sys.exit(1)

    # Find input files
    cpp_file = input_dir / 'exprtk_test.cpp'
    func_file = input_dir / 'exprtk_functional_test.txt'
    ext_file = input_dir / 'exprtk_functional_ext_test.txt'

    stats = ConversionStats()

    # Process exprtk_test.cpp
    corpus_tests = []
    if cpp_file.exists():
        corpus_tests = process_file(cpp_file, stats)
    else:
        logger.warning(f"File not found: {cpp_file}")

    # Process functional test files
    func_tests = []
    if func_file.exists():
        func_tests = process_file(func_file, stats)
    else:
        logger.warning(f"File not found: {func_file}")

    # Process extended functional test file
    ext_tests = []
    if ext_file.exists():
        ext_tests = process_file(ext_file, stats)
    else:
        logger.warning(f"File not found: {ext_file}")

    # Print statistics
    logger.info("")
    logger.info("=" * 60)
    logger.info("Conversion Statistics")
    logger.info("=" * 60)
    logger.info(f"Total tests parsed:     {stats.total}")
    logger.info(f"Tests converted:        {stats.converted}")
    logger.info(f"Tests filtered:         {stats.filtered}")
    logger.info("")
    logger.info("Filter reasons:")
    for reason, count in sorted(stats.filter_reasons.items(), key=lambda x: -x[1]):
        logger.info(f"  {reason}: {count}")

    if args.dry_run:
        logger.info("")
        logger.info("Dry run - no output files written")
        return

    # Generate output files
    output_dir = args.output_dir
    if not output_dir.is_absolute():
        output_dir = Path.cwd() / output_dir

    output_dir.mkdir(parents=True, exist_ok=True)

    logger.info("")
    logger.info("Generating output files...")

    # Generate corpus tests
    if corpus_tests:
        out_path = output_dir / 'exprtk_corpus_tests.cc'
        count = generate_test_file(corpus_tests, out_path, 'ExprtkCorpus', 'Corpus')
        logger.info(f"  {out_path.name}: {count} tests")

    # Generate functional tests
    if func_tests:
        out_path = output_dir / 'exprtk_functional_tests.cc'
        count = generate_test_file(func_tests, out_path, 'ExprtkFunctional', 'Functional')
        logger.info(f"  {out_path.name}: {count} tests")

    # Generate extended tests (split into parts)
    if ext_tests:
        TESTS_PER_FILE = 12000
        num_parts = (len(ext_tests) + TESTS_PER_FILE - 1) // TESTS_PER_FILE

        for part_idx in range(num_parts):
            start_idx = part_idx * TESTS_PER_FILE
            end_idx = min((part_idx + 1) * TESTS_PER_FILE, len(ext_tests))
            part_tests = ext_tests[start_idx:end_idx]

            part_num = part_idx + 1
            out_path = output_dir / f'exprtk_ext_tests_part{part_num}.cc'
            count = generate_test_file(
                part_tests, out_path,
                f'ExprtkExtPart{part_num}',
                f'ExtPart{part_num}'
            )
            logger.info(f"  {out_path.name}: {count} tests")

    logger.info("")
    logger.info("Done!")


if __name__ == '__main__':
    main()
