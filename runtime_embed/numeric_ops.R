# Numeric utility functions
# These are basic numeric operations that don't fit in vector_ops
#
# The compiler automatically dispatches max(a, b) to the correct overload
# based on argument types using name mangling.
#
# Naming scheme: system_name___arg1__arg2__arg3
#   - THREE underscores (___) between function name and first arg
#   - TWO underscores (__) between arguments

# ============================================================================
# MAX operations for scalars
# ============================================================================

# max(int, int) -> int
system_max___int__int <- function(a: int, b: int): int {
    result <- a
    if(b > a) {
        result <- b
    }
    return(result)
}

# max(double, double) -> double
system_max___double__double <- function(a: double, b: double): double {
    result <- a
    if(b > a) {
        result <- b
    }
    return(result)
}

system_min___int__int <- function(a: int, b: int): int {
    result <- a
    if(b < a) {
        result <- b
    }
    return(result)
}

# min(double, double) -> double
system_min___double__double <- function(a: double, b: double): double {
    result <- a
    if(b < a) {
        result <- b
    }
    return(result)
}

# ============================================================================
# MAX operations for vectors
# ============================================================================

# max(vector<int>) -> int
system_max___vec_int <- function(x: vector<int>): int {
    n <- length(x)
    if(n == 0L) {
        stop("Cannot compute max of empty vector")
    }

    result <- x[1L]
    i <- 2L
    while(i <= n) {
        if(x[i] > result) {
            result <- x[i]
        }
        i <- i + 1L
    }
    return(result)
}

# max(vector<double>) -> double
system_max___vec_double <- function(x: vector<double>): double {
    n <- length(x)
    if(n == 0) {
        stop("Cannot compute max of empty vector")
    }

    result <- x[1]
    i <- 2
    while(i <= n) {
        if(x[i] > result) {
            result <- x[i]
        }
        i <- i + 1
    }
    return(result)
}

# ============================================================================
# MIN operations for vectors
# ============================================================================

# min(vector<int>) -> int
system_min___vec_int <- function(x: vector<int>): int {
    n <- length(x)
    if(n == 0L) {
        stop("Cannot compute min of empty vector")
    }

    result <- x[1L]
    i <- 2L
    while(i <= n) {
        if(x[i] < result) {
            result <- x[i]
        }
        i <- i + 1L
    }
    return(result)
}

# min(vector<double>) -> double
system_min___vec_double <- function(x: vector<double>): double {
    n <- length(x)
    if(n == 0) {
        stop("Cannot compute min of empty vector")
    }

    result <- x[1]
    i <- 2
    while(i <= n) {
        if(x[i] < result) {
            result <- x[i]
        }
        i <- i + 1
    }
    return(result)
}
