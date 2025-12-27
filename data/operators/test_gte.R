# Test >= operator with vectors
x <- c(5, 3, 7)
y <- c(3, 4, 5)
result <- x >= y

# Test with scalars
a <- 5
b <- 3
result2 <- a >= b

# Test vector-scalar
vec <- 1:5
result3 <- vec >= 3

# Test scalar-vector
result4 <- 2 >= vec
