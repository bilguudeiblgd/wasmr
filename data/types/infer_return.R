# Test return type inference from tail expression

# Explicitly typed - should still work
add_explicit <- function(x: int, y: int): int {
    x + y
}

# Inferred from tail expression - should infer int
add_inferred <- function(x: int, y: int) {
    x + y
}

# Inferred from tail expression - should infer logical
is_positive <- function(x: int) {
    x > 0
}

# No tail expression - should default to Any
do_nothing <- function() {
    x: int <- 5
}

result1: int <- add_explicit(3, 4)
print(result1)

result2: int <- add_inferred(10, 20)
print(result2)

result3: logical <- is_positive(5)
print(result3)
