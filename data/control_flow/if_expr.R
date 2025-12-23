# Test if expression with simple values
x: int <- 15
my_val: int <- if(x > 10) 1 else 2
print(my_val)

# Test if expression with blocks
y: int <- if(x < 10) {
    print(0)
    100
} else {
    print(1)
    200
}
print(y)
