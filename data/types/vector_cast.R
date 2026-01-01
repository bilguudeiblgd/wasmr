# Test explicit vector casting functions
v_int: vector<int> <- c(1L, 2L, 3L)
v_double: vector<double> <- as.double(v_int)
print(length(v_double))

v_dbl: vector<double> <- c(1.5, 2.5, 3.5)
v_int2: vector<int> <- as.integer(v_dbl)
print(length(v_int2))

v_log: vector<logical> <- as.logical(c(1L, 0L, 1L))
v_int3: vector<int> <- as.integer(v_log)
print(length(v_int3))
