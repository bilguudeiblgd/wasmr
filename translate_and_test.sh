#!/bin/bash
set -e
rm -rf data_R
echo "Building and running type eraser..."
cargo run --bin type_eraser

echo "Running tests..."
./test.sh
