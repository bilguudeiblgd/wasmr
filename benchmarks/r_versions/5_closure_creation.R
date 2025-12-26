# Benchmark 5: Closure creation (10k)
# Create 10k closures that capture a variable

make_adder <- function(x) {
  adder <- function(y) {
    return(x + y)
  }
  return(adder)
}

n <- 10000
sum <- 0
i <- 1

while (i <= n) {
  f <- make_adder(i)
  sum <- sum + f(1)
  i <- i + 1
}

print(sum)
