f <- function() {
    x <- 1
    g <- function() {
    h <- function() {
    x <<- (x + 1)
    return(x)
}
    result <- h()
    return(result)
}
    result <- g()
    return(result)
}
result <- f()
print(result)
