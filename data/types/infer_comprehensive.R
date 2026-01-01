# Comprehensive return type inference test

# 1. Infer int from arithmetic
double_it <- function(x: int) {
    x * 2L
}

# 2. Infer logical from comparison
is_even <- function(n: int) {
    n == 2L
}

# 3. Infer double from literal
get_pi <- function() {
    3.14
}

# 4. Infer int from addition
add_ten <- function(x: int) {
    x + 10L
}

# Test all functions
val1: int <- double_it(21L)
print(val1)

val2: logical <- is_even(2L)
print(val2)

val3: double <- get_pi()
print(val3)

val4: int <- add_ten(5L)
print(val4)
