# Simple closure test - R version
make_adder <- function(x) {
    inner <- function(y) {
        return(x + y)
    }
    return(inner)
}

add5 <- make_adder(5.0)
result <- add5(3.0)
print(result)
