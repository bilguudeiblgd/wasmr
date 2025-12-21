#!/bin/bash

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counters
PASSED=0
FAILED=0
SKIPPED=0

# Function to normalize floating point output
# Converts "2.50" to "2.5", handles both single numbers and multi-line output
normalize_floats() {
    local input="$1"
    echo "$input" | awk '{
        # Check if line is a number (int or float)
        if ($0 ~ /^-?[0-9]+(\.[0-9]+)?$/) {
            # Convert to float and print with minimal decimals
            printf "%.15g\n", $0
        } else {
            print $0
        }
    }'
}

echo "========================================="
echo "  Rty Compiler Test Suite"
echo "========================================="
echo ""

# Find all .R files recursively in data_R/ directory (plain R versions for testing)
while IFS= read -r -d '' plain_r_file; do
    # Compute relative path from data_R/
    rel_path="${plain_r_file#data_R/}"
    rel_path_no_ext="${rel_path%.R}"

    # Corresponding paths
    rty_file="data/${rel_path}"
    wasm_file="out/${rel_path_no_ext}.wasm"

    # Display name for test output
    test_name="${rel_path_no_ext}"

    # Skip if corresponding Rty file doesn't exist
    if [ ! -f "$rty_file" ]; then
        echo -e "${YELLOW}[SKIP]${NC} $test_name - no corresponding Rty file in data/"
        ((SKIPPED++))
        continue
    fi

    # Skip if WASM file doesn't exist
    if [ ! -f "$wasm_file" ]; then
        echo -e "${YELLOW}[SKIP]${NC} $test_name - no WASM file"
        ((SKIPPED++))
        continue
    fi

    # Run plain R script and capture output, removing [1] prefix
    r_output=$(Rscript "$plain_r_file" 2>&1 | sed 's/^\[1\] //')
    r_exit=$?

    # Skip if R script failed to run
    if [ $r_exit -ne 0 ]; then
        echo -e "${RED}[FAIL]${NC} $test_name - R script failed"
        echo "  R output: $r_output"
        ((FAILED++))
        continue
    fi

    # Run WASM and capture output
    wasm_output=$(wasmtime -W gc=y -W function-references "$wasm_file" 2>&1)
    wasm_exit=$?

    # Skip if WASM failed to run
    if [ $wasm_exit -ne 0 ]; then
        echo -e "${YELLOW}[SKIP]${NC} $test_name - WASM failed to execute"
        ((SKIPPED++))
        continue
    fi

    # Normalize floating point numbers in both outputs
    r_output_normalized=$(normalize_floats "$r_output")
    wasm_output_normalized=$(normalize_floats "$wasm_output")

    # Compare normalized outputs
    if [ "$r_output_normalized" = "$wasm_output_normalized" ]; then
        echo -e "${GREEN}[PASS]${NC} $test_name"
        ((PASSED++))
    else
        echo -e "${RED}[FAIL]${NC} $test_name"
        echo "  Expected (R):  $r_output"
        echo "  Got (WASM):    $wasm_output"
        echo "  (Normalized comparison failed)"
        ((FAILED++))
    fi
done < <(find data_R -type f -name "*.R" -print0)

echo ""
echo "========================================="
echo "  Results"
echo "========================================="
echo -e "${GREEN}Passed:${NC}  $PASSED"
echo -e "${RED}Failed:${NC}  $FAILED"
echo -e "${YELLOW}Skipped:${NC} $SKIPPED"
echo ""

# Exit with failure if any tests failed
if [ $FAILED -gt 0 ]; then
    exit 1
fi
