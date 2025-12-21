twice <- function(x) {
    return((x * 2))
}
apply <- function(f, x) {
    return(f(x))
}
result <- apply(twice, 5)
print(result)
