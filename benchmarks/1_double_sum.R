# Benchmark 1: Integer sum (10k elements)
# Sum all integers from 1 to 10000

sum_integers <- function(n: double): double {
  total: double <- 0
  i: double <- 1
  while (i <= n) {
    total <- total + i
    i <- i + 1
  }
  return(total)
}

result: double <- sum_integers(10000)
print(result)