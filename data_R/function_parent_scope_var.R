# data/functions.R

# Square and linear combination as simple algorithms
c <- 1
square_and_add_one <- function(x) {
  return(x * x + c)
}

# Use the functions
sq9 <- square_and_add_one(9)
print(sq9)
