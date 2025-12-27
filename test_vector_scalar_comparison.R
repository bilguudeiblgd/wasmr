# Test vector-scalar comparisons
vec <- 1:5
result <- vec < 3
print(result)

# Test scalar-vector
result2 <- 2 > vec
print(result2)

# Test with different types
vec_d <- c(1.5, 2.5, 3.5)
result3 <- vec_d >= 2.0
print(result3)
