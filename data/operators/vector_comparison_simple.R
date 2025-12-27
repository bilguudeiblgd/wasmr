# Simple vector comparison test without type annotations

vi1 <- c(1, 2, 3)
vi2 <- c(2, 1, 4)
result1 <- vi1 < vi2
print(result1[1])

vd1 <- c(1.5, 2.5, 3.5)
vd2 <- c(1.0, 3.0, 3.5)
result2 <- vd1 >= vd2
print(result2[1])
