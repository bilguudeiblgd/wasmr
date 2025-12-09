# Simple closure example - counter function
make_counter <- function() {
    count <- 0

    increment <- function() {
        count <<- count + 1
        count
    }

    increment
}

counter <- make_counter()
print(counter())
print(counter())
print(counter())
