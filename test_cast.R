# Test vector and scalar casting

# Test 1: vector<int> to vector<double>
vi: vector<int> <- c(1, 2, 3)
vd: vector<double> <- vi  # Should call system_cast_vec_int_to_vec_double
print(vd[1])

# Test 2: vector<double> to vector<int>
vd2: vector<double> <- c(1.5, 2.7, 3.2)
vi2: vector<int> <- vd2  # Should call system_cast_vec_double_to_vec_int
print(vi2[1])

# Test 3: scalar double to logical
d: double <- 5.0
b: logical <- d  # Should be TRUE (non-zero)
print(b)

# Test 4: scalar int to logical
i: int <- 0
b2: logical <- i  # Should be FALSE (zero)
print(b2)
