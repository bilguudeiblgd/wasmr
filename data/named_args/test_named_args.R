# Test named arguments and default parameters

# Function with default parameters
greet <- function(name: string, greeting: string = "Hello") {
    print(greeting)
    print(name)
}

# Call with all positional arguments
greet("World", "Hi")

# Call with named arguments
greet(name="Alice", greeting="Howdy")

# Call with mix (positional first, then named)
greet("Bob", greeting="Hey")
