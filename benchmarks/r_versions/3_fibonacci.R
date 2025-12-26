# Benchmark 3: Recursive Fibonacci(25)
# Compute fibonacci number recursively

fib <- function(n) {
  if (n <= 1) {
    return(n)
  }
  return(fib(n - 1) + fib(n - 2))
}

result <- fib(25)
print(result)
