# data/math.R

# Arithmetic with grouping and mixed precedence
x: int <- 10
y: int <- 3
sum_xy: int <- x + y
prod_xy: int <- x * y
expr1: int <- x + y * 2
expr2: int <- (x + y) * 2
rng: int <- 1:5

# Simple function that adds three ints
add3 <- function(a: int, b: int, c: int): int {
  return(a + b + c)
}

z: int <- add3(1, 2, 3)