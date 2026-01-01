# Test first-class functions: passing function as argument

twice <- function(x: int): int {
    return(x * 2L)
}

apply <- function(f: int -> int, x: int): int {
    return(f(x))
}

result <- apply(twice, 5L)
print(result)

