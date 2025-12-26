# Benchmark 1: Integer sum (10k elements)
# Sum all integers from 1 to 10000

sum_integers <- function(n: int): int {
  total: int <- 0
  i: int <- 1
  while (i <= n) {
    total <- total + i
    i <- i + 1
  }
  return(total)
}

result: int <- sum_integers(10000)
print(result)