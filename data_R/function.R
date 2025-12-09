# data/functions.R

# Square and linear combination as simple algorithms
c <- 1
square_and_add_one <- function(x) {
  return(x * x + c)
}

lincomb <- function(a, b, x, y) {
  return(a * x + b * y)
}

# Use the functions
sq9 <- square_and_add_one(9)
lc <- lincomb(2, 3, 5, 7)
print(sq9)
print(lc)