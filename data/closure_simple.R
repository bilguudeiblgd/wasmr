# Simple closure test - function capturing a variable from parent scope
make_adder <- function(x: double): double -> double {
    inner <- function(y: double): double {
        return(x + y)
    }
    return(inner)
}

add5 <- make_adder(5.0)
result: int <- add5(3.0)
print(result)
