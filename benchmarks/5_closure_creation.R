# Benchmark 5: Closure creation (10k)
# Create 10k closures that capture a variable

make_adder <- function(x: int): (int -> int) {
  adder <- function(y: int): int {
    return(x + y)
  }
  return(adder)
}

n: int <- 10000
sum: int <- 0
i: int <- 1

while (i <= n) {
  f: (int -> int) <- make_adder(i)
  sum <- sum + f(1)
  i <- i + 1
}

print(sum)
