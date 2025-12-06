# data/functions.R

# Square and linear combination as simple algorithms
square <- function(x) {
  return(x * x)
}

lincomb <- function(a, b, x, y) {
  return(a * x + b * y)
}

# Use the functions
sq9 <- square(9)
lc <- lincomb(2, 3, 5, 7)
print(sq9)