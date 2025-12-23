# Test boolean literals and operations

# Test basic boolean literals
t <- TRUE
f <- FALSE

# Test boolean operations
and_result <- t & f
or_result <- t | f

# Test comparisons producing booleans
comp1 <- 5 > 3
comp2 <- 2 < 1

# Test boolean in if statement
if (TRUE) {
    x <- 10
} else {
    x <- 20
}

# Test logical operations
bool_and <- TRUE & FALSE
bool_or <- TRUE | FALSE

# Test function with boolean parameters and return
is_positive <- function(n: int): bool {
    return (n > 0)
}

result <- is_positive(5)
