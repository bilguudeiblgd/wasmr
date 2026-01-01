# Test named arguments with simple types

# Function with default parameters
add <- function(x: int, y: int = 5L) : int {
    return(x + y)
}

# Call with all positional arguments
result1 <- add(10L, 20L)
print(result1)

# Call with named argument
result2 <- add(x=15L, y=25L)
print(result2)

# Call with positional and named mix
result3 <- add(30L, y=40L)
print(result3)
