# Test if expression with simple values
x: double <- 15
my_val: double <- if(x > 10) 1 else 2
print(my_val)

# Test if expression with blocks
y: double <- if(x < 10) {
    print(0)
    100
} else {
    print(1)
    200
}
print(y)
