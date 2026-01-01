# Test explicit scalar casting functions
x: int <- 42L
y: double <- as.double(x)
print(y)

z: double <- 3.14
w: int <- as.integer(z)
print(w)

a: int <- 1L
b: logical <- as.logical(a)
print(b)

c: logical <- 0L
d: int <- as.integer(c)
print(d)
