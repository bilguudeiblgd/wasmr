#!/bin/bash
set -e

echo "Building and running type eraser..."
cargo run --bin type_eraser

echo "Running tests..."
./test.sh
