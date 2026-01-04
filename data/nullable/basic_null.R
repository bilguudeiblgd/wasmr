# Basic NULL functionality test

# Test 1: NULL literal
x <- NULL
print(is.null(x))  # Should print TRUE

# Test 2: Non-NULL value
y <- 42L
print(is.null(y))  # Should print FALSE

# Test 3: Nullable type annotation with NULL
z: int? <- NULL
print(is.null(z))  # Should print TRUE

# Test 4: Nullable type with non-NULL value
w: int? <- 10L
print(is.null(w))  # Should print FALSE
