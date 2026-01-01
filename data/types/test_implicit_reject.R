# This should fail: implicit vector casting in assignment
v_double: vector<double> <- c(1.0, 2.0, 3.0)
v_int: vector<int> <- v_double  # Should be rejected!
