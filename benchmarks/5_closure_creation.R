# Benchmark 5: Closure creation (10k)
# Create 10k closures that capture a variable

make_adder <- function(x: double): (double -> double) {
  adder <- function(y: double): double {
    return(x + y)
  }
  return(adder)
}

n: double <- 10000
sum: double <- 0
i: double <- 1

while (i <= n) {
  f: (double -> double) <- make_adder(i)
  sum <- sum + f(1)
  i <- i + 1
}

print(sum)
