# Simple closure example - counter function with types
make_counter <- function(): () -> int {
    count: int <- 0

    increment <- function(): int {
        count <<- count + 1
        return(count)
    }

    return(increment)
}

counter: () -> int <- make_counter()
print(counter())
print(counter())
print(counter())
