# Test tail expressions in nested blocks
compute <- function(x: int): int {
    {
        temp <- x * 2
        temp + 5
    }
}

result: int <- compute(10)
print(result)
