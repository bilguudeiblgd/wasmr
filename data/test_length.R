# Test length() builtin with struct-based vectors

# Create a vector using c()
v <- c(1, 2, 3, 4, 5)

# Test length() - should print 5
len <- length(v)
print(len)

# Test with range operator
r <- 1:10
len_r <- length(r)
print(len_r)

# Test vector indexing still works
elem <- v[1]
print(elem)

# Test for loop still works
for (x in v) {
    print(x)
}
