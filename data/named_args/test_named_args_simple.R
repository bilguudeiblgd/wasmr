# Test named arguments with simple types

# Function with default parameters
add <- function(x: int, y: int = 5) : int {
    return(x + y)
}

# Call with all positional arguments
result1 <- add(10, 20)
print(result1)

# Call with named argument
result2 <- add(x=15, y=25)
print(result2)

# Call with positional and named mix
result3 <- add(30, y=40)
print(result3)
