# Test first-class functions: passing function as argument

twice <- function(x: int): int {
    return(x * 2)
}

apply <- function(f: int -> int, x: int): int {
    return(f(x))
}

result <- apply(twice, 5)
print(result)

