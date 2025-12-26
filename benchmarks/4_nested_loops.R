# Benchmark 4: Nested loops (1M iterations)
# Nested loop that runs 1M times total

outer: int <- 1000
inner: int <- 1000
count: int <- 0

i: int <- 1
while (i <= outer) {
  j: int <- 1
  while (j <= inner) {
    count <- count + 1
    j <- j + 1
  }
  i <- i + 1
}

print(count)
