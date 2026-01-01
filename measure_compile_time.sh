#!/bin/bash

# Script to measure compilation times

echo "=== Compilation Time Benchmarks ==="
echo ""

# Function to measure compilation time
measure_compile() {
    local file=$1
    local desc=$2
    local lines=$(wc -l < "$file" | tr -d ' ')

    # Warmup
    MY_FILE="$file" cargo run --release --quiet >/dev/null 2>&1

    # Measure 5 times and take average
    total=0
    for i in {1..5}; do
        start=$(ruby -e 'puts (Time.now.to_f * 1000).to_i')
        MY_FILE="$file" cargo run --release --quiet >/dev/null 2>&1
        end=$(ruby -e 'puts (Time.now.to_f * 1000).to_i')
        elapsed=$((end - start))
        total=$((total + elapsed))
    done
    avg=$((total / 5))

    printf "%-30s | %4d lines | %4d ms\n" "$desc" "$lines" "$avg"
}

# Individual files
measure_compile "data/basic/add.R" "basic/arithmetic.R"
measure_compile "data/functions/first_class_function.R" "functions/factorial.R"
measure_compile "data/closures/generator.R" "closures/counter.R"
measure_compile "data/vectors/sum_vector.R" "vectors/operations.R"

echo ""
echo "Full suite compilation:"

# Count all .R files
num_files=$(find data -name "*.R" | wc -l | tr -d ' ')
total_lines=$(find data -name "*.R" -exec wc -l {} + | tail -1 | awk '{print $1}')

# Measure full suite compilation
start=$(ruby -e 'puts (Time.now.to_f * 1000).to_i')
cargo run --release --quiet >/dev/null 2>&1
end=$(ruby -e 'puts (Time.now.to_f * 1000).to_i')
elapsed=$((end - start))

printf "Full suite (%d files) | %4d lines | %4d ms\n" "$num_files" "$total_lines" "$elapsed"

echo ""
echo "Compilation complete!"
