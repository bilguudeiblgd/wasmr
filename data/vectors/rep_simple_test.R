# Simple rep test with positional arguments only

# Test with positional args
v1: vector<int> <- c(1L, 2L, 3L)
r1: vector<int> <- rep(v1, 2L, 1L)
print(length(r1))  # Expected: 6
print(r1[1])       # Expected: 1
print(r1[4])       # Expected: 1
