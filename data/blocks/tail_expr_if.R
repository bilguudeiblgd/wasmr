# Test tail expressions in if/else blocks
max_value <- function(a: int, b: int): int {
    if(a > b) {
        print(1)
        a
    } else {
        print(2)
        b
    }
}

result: int <- max_value(15, 8)
print(result)
