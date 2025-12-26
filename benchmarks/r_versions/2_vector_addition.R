# Benchmark 2: Vector addition (10k elements)
# Add two vectors element-wise

n <- 10000

# Create vector using range
v1 <- 1:n
v2 <- 1:n
# Add vector to itself
result <- v1 + v2

# Sum result to get a single output
sum <- 0
j <- 1
while (j <= n) {
  sum <- sum + result[j]
  j <- j + 1
}

print(sum)
