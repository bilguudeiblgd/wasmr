# Test operator precedence

# Multiplication before addition: 2 + 3 * 4 = 2 + 12 = 14
result1 <- 2L + 3L * 4L
print(result1)

# Division before subtraction: 10 - 8 / 2 = 10 - 4 = 6
result2 <- 10L - 8L / 2L
print(result2)

# Comparison after arithmetic: 5 + 3 > 2 * 4 = 8 > 8 = FALSE
result3 <- 5L + 3L > 2L * 4L
print(result3)
