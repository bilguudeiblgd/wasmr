# Benchmark 4: Nested loops (1M iterations)
# Nested loop that runs 1M times total

outer <- 1000
inner <- 1000
count <- 0

i <- 1
while (i <= outer) {
  j <- 1
  while (j <= inner) {
    count <- count + 1
    j <- j + 1
  }
  i <- i + 1
}

print(count)
