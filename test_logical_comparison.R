# Test logical with int
a <- TRUE
b <- 5
result1 <- a < b  # TRUE (1) < 5

# Test logical with double
c <- FALSE
d <- 3.5
result2 <- c < d  # FALSE (0) < 3.5

# Test vector<logical> with vector<int>
vec_log <- c(TRUE, FALSE, TRUE)
vec_int <- c(2, 3, 1)
result3 <- vec_log < vec_int  # c(1, 0, 1) < c(2, 3, 1)

# Test vector<logical> with int scalar
result4 <- vec_log < 2  # c(1, 0, 1) < 2

# Test logical scalar with vector<int>
result5 <- TRUE > vec_int  # 1 > c(2, 3, 1)
