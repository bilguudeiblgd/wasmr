# Simple closure example - counter function with types
make_counter <- function(): () -> int {
    count: int <- 0L

    increment <- function(): int {
        count <<- count + 1L
        return(count)
    }

    return(increment)
}

counter: () -> int <- make_counter()
print(counter())
print(counter())
print(counter())
