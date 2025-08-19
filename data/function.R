# data/functions.R

# Square and linear combination as simple algorithms
square <- function(x: int): int {
  return(x * x)
}

lincomb <- function(a: int, b: int, x: int, y: int): int {
  return(a * x + b * y)
}

# Use the functions
sq9: int <- square(9)
lc: int <- lincomb(2, 3, 5, 7)