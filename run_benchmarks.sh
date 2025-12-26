#!/bin/bash

# Benchmark runner script
# Compares Rty/WASM performance against R/Rscript

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

BENCHMARK_DIR="benchmarks"
R_VERSIONS_DIR="$BENCHMARK_DIR/r_versions"
OUT_DIR="out/benchmarks"
WARMUP_RUNS=2
BENCH_RUNS=5

# Create output directory
mkdir -p "$OUT_DIR"

echo -e "${BLUE}=== Rty/WASM vs R Performance Benchmark ===${NC}\n"
echo "Warmup runs: $WARMUP_RUNS"
echo "Benchmark runs: $BENCH_RUNS"
echo ""

# Function to compile Rty file to WASM
compile_rty() {
    local rty_file=$1
    local base_name=$(basename "$rty_file" .R)
    local wasm_out="$OUT_DIR/${base_name}.wasm"

    # Compile using our compiler (suppress all output)
    MY_FILE="$rty_file" cargo run --bin Rty_compiler --quiet >/dev/null 2>&1

    # Check if compilation succeeded (compiler outputs directly to OUT_DIR)
    if [ -f "$wasm_out" ]; then
        echo "$wasm_out"
    else
        echo ""
    fi
}

# Function to run benchmark and measure time
run_benchmark() {
    local cmd=$1
    local runs=$2
    local times=()

    for ((i=1; i<=runs; i++)); do
        # Capture time in milliseconds
        start=$(ruby -e 'puts (Time.now.to_f * 1000).to_i')
        eval "$cmd" > /dev/null 2>&1
        end=$(ruby -e 'puts (Time.now.to_f * 1000).to_i')
        elapsed=$((end - start))
        times+=($elapsed)
    done

    # Calculate average
    sum=0
    for t in "${times[@]}"; do
        sum=$((sum + t))
    done
    avg=$((sum / runs))
    echo $avg
}

# Array to store results
declare -a results

# Benchmark files
benchmarks=(
    "1_integer_sum:Integer sum (10k elements)"
    "2_vector_addition:Vector addition (10k)"
    "3_fibonacci:Recursive Fibonacci(25)"
    "4_nested_loops:Nested loops (1M iterations)"
    "5_closure_creation:Closure creation (10k)"
)

echo -e "${YELLOW}Building compiler...${NC}"
cargo build --release --quiet 2>&1 | grep -v "Finished\|Compiling" || true
echo ""

for bench in "${benchmarks[@]}"; do
    IFS=':' read -r file_base description <<< "$bench"

    echo -e "${BLUE}Benchmarking: ${description}${NC}"

    # Files
    rty_file="$BENCHMARK_DIR/${file_base}.R"
    r_file="$R_VERSIONS_DIR/${file_base}.R"

    # Check if files exist
    if [ ! -f "$rty_file" ]; then
        echo -e "${RED}  ERROR: Rty file not found: $rty_file${NC}"
        continue
    fi
    if [ ! -f "$r_file" ]; then
        echo -e "${RED}  ERROR: R file not found: $r_file${NC}"
        continue
    fi

    # Compile Rty to WASM
    echo "  Compiling Rty to WASM..."
    wasm_file=$(compile_rty "$rty_file")

    if [ -z "$wasm_file" ]; then
        echo -e "${RED}  ERROR: Compilation failed${NC}"
        continue
    fi

    # Verify outputs match
    echo "  Verifying outputs..."
    r_output=$(Rscript "$r_file" 2>/dev/null | tail -1)
    wasm_output=$(wasmtime -W gc=y -W function-references=y "$wasm_file" 2>/dev/null | tail -1)

    # Clean R output (remove [1] prefix)
    r_output_clean=$(echo "$r_output" | sed 's/\[1\] //')

    echo "    R output:    $r_output"
    echo "    WASM output: $wasm_output"

    if [ "$r_output_clean" = "$wasm_output" ]; then
        echo -e "    ${GREEN}✓ Outputs match${NC}"
    else
        echo -e "    ${YELLOW}⚠ Outputs differ (may be formatting)${NC}"
    fi

    # Warmup runs
    echo "  Warming up..."
    for ((i=1; i<=WARMUP_RUNS; i++)); do
        Rscript "$r_file" > /dev/null 2>&1 || true
        wasmtime -W gc=y -W function-references=y "$wasm_file" > /dev/null 2>&1 || true
    done

    # Benchmark R
    echo "  Benchmarking Rscript..."
    r_time=$(run_benchmark "Rscript '$r_file'" $BENCH_RUNS)

    # Benchmark WASM
    echo "  Benchmarking Rty/WASM..."
    wasm_time=$(run_benchmark "wasmtime -W gc=y -W function-references=y '$wasm_file'" $BENCH_RUNS)

    # Calculate speedup
    speedup=$(echo "scale=2; $r_time / $wasm_time" | bc)

    # Store results
    results+=("$description|$r_time|$wasm_time|$speedup")

    echo -e "  ${GREEN}R: ${r_time}ms, WASM: ${wasm_time}ms, Speedup: ${speedup}x${NC}\n"
done

# Print summary table
echo ""
echo -e "${BLUE}=== BENCHMARK RESULTS ===${NC}"
echo ""
printf "%-35s | %-10s | %-13s | %-8s\n" "Benchmark" "R (ms)" "Rty/WASM (ms)" "Speedup"
echo "--------------------------------------------------------------------------------"

for result in "${results[@]}"; do
    IFS='|' read -r desc r_time wasm_time speedup <<< "$result"
    printf "%-35s | %-10s | %-13s | %-8s\n" "$desc" "$r_time" "$wasm_time" "${speedup}×"
done

echo ""
echo -e "${GREEN}Benchmark complete!${NC}"
