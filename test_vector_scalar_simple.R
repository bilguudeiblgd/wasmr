# Test vector-scalar comparisons (just compilation)
vec <- 1:5
result <- vec < 3

# Test scalar-vector
result2 <- 2 > vec

# Test with doubles
vec_d <- c(1.5, 2.5, 3.5)
result3 <- vec_d >= 2.0

# Test different operators
result4 <- vec <= 4
result5 <- 3 == vec
result6 <- vec != 2
