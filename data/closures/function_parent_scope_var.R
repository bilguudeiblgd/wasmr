# data/functions.R

# Square and linear combination as simple algorithms
c <- 1L
square_and_add_one <- function(x: int): int {
  return(x * x + c)
}

# Use the functions
sq9: int <- square_and_add_one(9L)
print(sq9)