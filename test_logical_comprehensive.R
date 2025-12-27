# Comprehensive logical type comparison tests

# 1. Logical with Logical
a <- TRUE
b <- FALSE
result1 <- a < b  # Should promote to int: 1 < 0 = FALSE

# 2. Logical with Int
c <- TRUE
d <- 5
result2 <- c < d  # TRUE (1) < 5 = TRUE

# 3. Logical with Double
e <- FALSE
f <- 2.5
result3 <- e < f  # FALSE (0.0) < 2.5 = TRUE

# 4. Vector<Logical> with Vector<Logical>
vec1 <- c(TRUE, FALSE, TRUE)
vec2 <- c(FALSE, TRUE, FALSE)
result4 <- vec1 > vec2  # c(1,0,1) > c(0,1,0)

# 5. Vector<Logical> with Vector<Int>
vec_log <- c(TRUE, FALSE, TRUE)
vec_int <- c(0, 1, 2)
result5 <- vec_log >= vec_int  # c(1,0,1) >= c(0,1,2)

# 6. Vector<Logical> with Vector<Double>
vec_log2 <- c(FALSE, TRUE, TRUE)
vec_double <- c(1.5, 0.5, 2.0)
result6 <- vec_log2 <= vec_double  # c(0.0,1.0,1.0) <= c(1.5,0.5,2.0)

# 7. Vector<Logical> with Int scalar
vec_l <- c(TRUE, FALSE, TRUE)
result7 <- vec_l < 2  # c(1,0,1) < c(2,2,2)

# 8. Vector<Logical> with Double scalar
result8 <- vec_l > 0.5  # c(1.0,0.0,1.0) > c(0.5,0.5,0.5)

# 9. Logical scalar with Vector<Int>
result9 <- TRUE == vec_int  # c(1,1,1) == c(0,1,2)

# 10. Logical scalar with Vector<Double>
result10 <- FALSE != vec_double  # c(0.0,0.0,0.0) != c(1.5,0.5,2.0)
