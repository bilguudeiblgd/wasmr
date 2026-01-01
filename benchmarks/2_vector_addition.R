# Benchmark 2: Vector addition (10k elements)
# Add two vectors element-wise

n: double <- 10000

# Create vector using range
v1: vector<double> <- 1:n

# Add vector to itself
result: vector<double> <- v1 + v1

# Sum result to get a single output
sum: double <- 0
j: double <- 1
while (j <= n) {
  sum <- sum + result[j]
  j <- j + 1
}

print(sum)