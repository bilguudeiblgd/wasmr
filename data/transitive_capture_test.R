# Test transitive capture propagation
# Function f defines x, function g doesn't use x but h needs it
# g must capture x to pass it to h

f <- function(): int {
  x: int <- 1

  g <- function(): int {
    # g doesn't directly use x

    h <- function(): int {
      # h needs x from f's scope
      x <<- x + 1
      return(x)
    }

    # g must call h, which needs x
    result: int <- h()
    return(result)
  }

  result: int <- g()
  return(result)
}

result: int <- f()
print(result)