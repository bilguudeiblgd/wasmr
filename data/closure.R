# Simple closure example - counter function with types
make_counter <- function(): function {
    count: int <- 0

    increment <- function(): int {
        count <<- count + 1
        return(count)
    }

    return(increment)
}

counter: function <- make_counter()
print(counter())
print(counter())
print(counter())
