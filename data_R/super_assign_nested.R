x <- 10
f <- function() {
    m <- function() {
    g <- function() {
    x <<- 10
}
    g()
}
    m()
}
f()
print(x)
