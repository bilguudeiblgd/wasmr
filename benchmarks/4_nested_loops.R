# Benchmark 4: Nested loops (1M iterations)
# Nested loop that runs 1M times total

outer: double <- 1000
inner: double <- 1000
count: double <- 0

i: double <- 1
while (i <= outer) {
  j: double <- 1
  while (j <= inner) {
    count <- count + 1
    j <- j + 1
  }
  i <- i + 1
}

print(count)
