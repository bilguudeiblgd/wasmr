# Test vector comparison operators

# Test 1: Integer vector comparison with <
vi1: vector<int> <- c(1L, 2L, 3L)
vi2: vector<int> <- c(2L, 1L, 4L)
result1: vector<logical> <- vi1 < vi2
print(result1[1])  # Should be TRUE
print(result1[2])  # Should be FALSE

# Test 2: Double vector comparison with >=
vd1: vector<double> <- c(1.5, 2.5, 3.5)
vd2: vector<double> <- c(1.0, 3.0, 3.5)
result2: vector<logical> <- vd1 >= vd2
print(result2[1])  # Should be TRUE
print(result2[3])  # Should be TRUE (equal)

# Test 3: Integer vector equality
vi3: vector<int> <- c(5L, 10L, 15L)
vi4: vector<int> <- c(5L, 20L, 15L)
result3: vector<logical> <- vi3 == vi4
print(result3[1])  # Should be TRUE

# Test 4: Double vector inequality
vd3: vector<double> <- c(1.0, 2.0, 3.0)
vd4: vector<double> <- c(1.0, 2.0, 3.0)
result4: vector<logical> <- vd3 != vd4
print(result4[1])  # Should be FALSE

# Test 5: Mixed type comparison (int vs double, should promote to double)
vi5: vector<int> <- c(1L, 2L, 3L)
vd5: vector<double> <- c(1.5, 1.5, 3.5)
result5: vector<logical> <- vi5 < vd5
print(result5[1])  # Should be TRUE (1.0 < 1.5)
