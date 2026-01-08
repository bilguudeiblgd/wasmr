
f <- function(a : int) {
  a
}

g <- function(a : int) {
  a
}

h <- function(v: int) {
  a <- f(v)
  b <- g(a + 3L)
  a + b
}

print(h(5L))