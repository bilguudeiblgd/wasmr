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

echo "========================================="
echo "  Rty Compiler Test Suite"
echo "========================================="
echo ""

# Find all .R files in data_R/ directory (plain R versions for testing)
for plain_r_file in data_R/*.R; do
    # Check if data_R directory exists and has files
    if [ ! -e "$plain_r_file" ]; then
        echo -e "${RED}Error: data_R/ directory not found or empty${NC}"
        echo "Please create data_R/ with plain R versions of your test files"
        exit 1
    fi

    # Get the base filename without path and extension
    basename=$(basename "$plain_r_file" .R)
    wasm_file="data/wasm_out/${basename}.wasm"
    rty_file="data/${basename}.R"

    # Skip if corresponding Rty file doesn't exist
    if [ ! -f "$rty_file" ]; then
        echo -e "${YELLOW}[SKIP]${NC} $basename - no corresponding Rty file in data/"
        ((SKIPPED++))
        continue
    fi

    # Skip if WASM file doesn't exist
    if [ ! -f "$wasm_file" ]; then
        echo -e "${YELLOW}[SKIP]${NC} $basename - no WASM file"
        ((SKIPPED++))
        continue
    fi

    # Run plain R script and capture output, removing [1] prefix
    r_output=$(Rscript "$plain_r_file" 2>&1 | sed 's/^\[1\] //')
    r_exit=$?

    # Skip if R script failed to run
    if [ $r_exit -ne 0 ]; then
        echo -e "${RED}[FAIL]${NC} $basename - R script failed"
        echo "  R output: $r_output"
        ((FAILED++))
        continue
    fi

    # Run WASM and capture output
    wasm_output=$(wasmtime -W gc=y -W function-references "$wasm_file" 2>&1)
    wasm_exit=$?

    # Skip if WASM failed to run
    if [ $wasm_exit -ne 0 ]; then
        echo -e "${YELLOW}[SKIP]${NC} $basename - WASM failed to execute"
        ((SKIPPED++))
        continue
    fi

    # Compare outputs
    if [ "$r_output" = "$wasm_output" ]; then
        echo -e "${GREEN}[PASS]${NC} $basename"
        ((PASSED++))
    else
        echo -e "${RED}[FAIL]${NC} $basename"
        echo "  Expected (R):  $r_output"
        echo "  Got (WASM):    $wasm_output"
        ((FAILED++))
    fi
done

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
