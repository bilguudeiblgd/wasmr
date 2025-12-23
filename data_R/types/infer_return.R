add_explicit <- function(x, y) {
    (x + y)
}
add_inferred <- function(x, y) {
    (x + y)
}
is_positive <- function(x) {
    (x > 0)
}
do_nothing <- function() {
    x <- 5
}
result1 <- add_explicit(3, 4)
print(result1)
result2 <- add_inferred(10, 20)
print(result2)
result3 <- is_positive(5)
print(result3)
