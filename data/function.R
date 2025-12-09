# data/functions.R

# Square and linear combination as simple algorithms
c <- 1
square_and_add_one <- function(x: int): int {
  return(x * x + c)
}

lincomb <- function(a: int, b: int, x: int, y: int): int {
  return(a * x + b * y)
}

# Use the functions
sq9: int <- square_and_add_one(9)
lc: int <- lincomb(2, 3, 5, 7)
print(sq9)
print(lc)