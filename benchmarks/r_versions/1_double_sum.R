# Benchmark 1: Double sum (10k elements)
# Sum all doubles from 1 to 10000

sum_integers <- function(n) {
  total <- 0
  i <- 1
  while (i <= n) {
    total <- total + i
    i <- i + 1
  }
  return(total)
}

result <- sum_integers(10000)
print(result)
