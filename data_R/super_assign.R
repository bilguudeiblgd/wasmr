x <- 10

f <- function() {
  x <<- 15
}
f()
print(x)

y <- 10
g <- function() {
  y <- 15
}
g()
print(y)