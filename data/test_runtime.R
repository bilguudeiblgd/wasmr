# Test using runtime vector operations
x <- c(1, 2, 3, 4, 5)
y <- c(10, 20, 30, 40, 50)

# This should call the runtime function system_vector_add___vec_int__vec_int
result <- system_vector_add___vec_int__vec_int(x, y)
print(result)

# Test sum function
sum_x <- system_vector_sum___vec_int(x)
print(sum_x)
