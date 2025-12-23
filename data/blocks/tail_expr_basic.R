# Test basic tail expression in function
add_and_triple <- function(x: int, y: int): int {
    sum <- x + y
    sum * 3
}

result: int <- add_and_triple(5, 3)
print(result)
