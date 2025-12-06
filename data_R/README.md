# data_R/

This directory contains **plain R versions** of the test files for validating compiler output.

## Purpose

Since `data/` contains Rty files with type annotations (e.g., `x: int <- 42`), which are not valid R syntax, we maintain plain R equivalents here for testing.

## Testing Workflow

1. Write your Rty code in `data/filename.R` with type annotations
2. Create a plain R version in `data_R/filename.R` without type annotations
3. Run `./test.sh` to compare:
   - Rscript output from `data_R/filename.R`
   - WASM output from `data/wasm_out/filename.wasm`

## Example

**data/while.R** (Rty with types):
```r
i: int <- 1
sum: int <- 0
while(i <= 5) {
    sum <- sum + i
    i <- i + 1
}
print(sum)
```

**data_R/while.R** (plain R):
```r
i <- 1
sum <- 0
while(i <= 5) {
    sum <- sum + i
    i <- i + 1
}
print(sum)
```

Both should produce the same output: `15`
