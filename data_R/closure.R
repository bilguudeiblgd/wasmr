make_counter <- function() {
    count <- 0
    increment <- function() {
    count <<- (count + 1)
    return(count)
}
    return(increment)
}
counter <- make_counter()
print(counter())
print(counter())
print(counter())
