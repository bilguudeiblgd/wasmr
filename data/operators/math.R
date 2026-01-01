# data/math.R

# Arithmetic with grouping and mixed precedence
x: int <- 10L
y: int <- 3L
sum_xy: int <- x + y
prod_xy: int <- x * y
expr1: int <- x + y * 2L
expr2: int <- (x + y) * 2L

# Simple function that adds three ints
add3 <- function(a: int, b: int, c: int): int {
  return(a + b + c)
}

z: int <- add3(1L, 2L, 3L)