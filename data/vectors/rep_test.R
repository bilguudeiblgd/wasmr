# Test rep() function with different parameters
# Note: Using positional arguments (times, each) since named arguments
# aren't supported for overloaded runtime functions yet

# Test 1: rep with times only (int vector)
# rep(c(1, 2, 3), times=2) -> c(1, 2, 3, 1, 2, 3)
v1: vector<int> <- c(1L, 2L, 3L)
r1: vector<int> <- rep(v1, 2L, 1L)
print(length(r1))  # Expected: 6
print(r1[1])       # Expected: 1
print(r1[4])       # Expected: 1

# Test 2: rep with each only (int vector)
# rep(c(1, 2, 3), each=2) -> c(1, 1, 2, 2, 3, 3)
v2: vector<int> <- c(1L, 2L, 3L)
r2: vector<int> <- rep(v2, 1L, 2L)
print(length(r2))  # Expected: 6
print(r2[1])       # Expected: 1
print(r2[2])       # Expected: 1
print(r2[3])       # Expected: 2

# Test 3: rep with both times and each (int vector)
# rep(c(1, 2), times=2, each=2) -> c(1, 1, 2, 2, 1, 1, 2, 2)
v3: vector<int> <- c(1L, 2L)
r3: vector<int> <- rep(v3, 2L, 2L)
print(length(r3))  # Expected: 8
print(r3[1])       # Expected: 1
print(r3[3])       # Expected: 2
print(r3[5])       # Expected: 1

# Test 4: rep with times only (double vector)
# rep(c(1.5, 2.5), times=3) -> c(1.5, 2.5, 1.5, 2.5, 1.5, 2.5)
v4: vector<double> <- c(1.5, 2.5)
r4: vector<double> <- rep(v4, 3L, 1L)
print(length(r4))  # Expected: 6
print(r4[1])       # Expected: 1.5
print(r4[3])       # Expected: 1.5

# Test 5: rep with each only (double vector)
# rep(c(1.5, 2.5), each=3) -> c(1.5, 1.5, 1.5, 2.5, 2.5, 2.5)
v5: vector<double> <- c(1.5, 2.5)
r5: vector<double> <- rep(v5, 1L, 3L)
print(length(r5))  # Expected: 6
print(r5[2])       # Expected: 1.5
print(r5[4])       # Expected: 2.5

# Test 6: rep with logical vector (using sum to verify)
# rep(c(TRUE, FALSE), times=2, each=2) -> c(T, T, F, F, T, T, F, F)
v6: vector<logical> <- c(TRUE, FALSE)
r6: vector<logical> <- rep(v6, 2L, 2L)
print(length(r6))  # Expected: 8
print(sum(r6))     # Expected: 4 (four TRUEs)

# Test 7: rep with default parameters (should just return copy)
v7: vector<int> <- c(10L, 20L)
r7: vector<int> <- rep(v7, 1L, 1L)
print(length(r7))  # Expected: 2
print(sum(r7))     # Expected: 30

# Test 8: rep with scalar int
# rep(2, 5) -> c(2, 2, 2, 2, 2)
r8: vector<int> <- rep(2L, 5L, 1L)
print(length(r8))  # Expected: 5
print(sum(r8))     # Expected: 10

# Test 9: rep with scalar double
# rep(3.5, 3) -> c(3.5, 3.5, 3.5)
r9: vector<double> <- rep(3.5, 3L, 1L)
print(length(r9))  # Expected: 3
print(r9[1])       # Expected: 3.5

# Test 10: rep with scalar logical
# rep(TRUE, 4) -> c(T, T, T, T)
r10: vector<logical> <- rep(TRUE, 4L, 1L)
print(length(r10))  # Expected: 4
print(sum(r10))     # Expected: 4

# Test 11: rep with scalar and each parameter
# rep(5, times=2, each=3) -> c(5, 5, 5, 5, 5, 5)
r11: vector<int> <- rep(5L, 2L, 3L)
print(length(r11))  # Expected: 6
print(sum(r11))     # Expected: 30
