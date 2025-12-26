# Benchmark 2: Vector addition (10k elements)
# Add two vectors element-wise

n: int <- 10000

# Create vector using range
v1: vector<int> <- 1:n

# Add vector to itself
result: vector<int> <- v1 + v1

# Sum result to get a single output
sum: int <- 0
j: int <- 1
while (j <= n) {
  sum <- sum + result[j]
  j <- j + 1
}

print(sum)