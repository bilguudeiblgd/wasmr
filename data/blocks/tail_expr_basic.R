# Test basic tail expression in function
add_and_triple <- function(x: double, y: double): double {
    sum <- x + y
    sum * 3
}

result: double <- add_and_triple(5, 3)
print(result)
