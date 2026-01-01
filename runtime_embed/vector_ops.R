# Generic vector operations for all numeric types
# Function names follow the mangling scheme: base___type1__type2
# where types are: vec_int, vec_double, int, double

# ============================================================================
# VECTOR + VECTOR operations
# ============================================================================

# vector<int> + vector<int>
system_vector_add___vec_int__vec_int <- function(a: vector<int>, b: vector<int>): vector<int> {
    n <- length(a)
    m <- length(b)

    if(n != m & n %% m != 0 & m %% n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result: vector<int> <- vec(length=result_len, mode="int")

    for(i in 1L:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] + b[b_idx]
    }
    return(result)
}

# vector<double> + vector<double>
system_vector_add___vec_double__vec_double <- function(a: vector<double>, b: vector<double>): vector<double> {
    n <- length(a)
    m <- length(b)

    if(n != m & n %% m != 0 & m %% n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result: vector<double> <- vec(length=result_len, mode="double")

    for(i in 1L:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] + b[b_idx]
    }
    return(result)
}

# ============================================================================
# VECTOR + SCALAR operations
# ============================================================================

# vector<int> + int
system_vector_add___vec_int__int <- function(a: vector<int>, b: int): vector<int> {
    return(system_vector_add___vec_int__vec_int(a, c(b)))
}

# vector<double> + double
system_vector_add___vec_double__double <- function(a: vector<double>, b: double): vector<double> {
    return(system_vector_add___vec_double__vec_double(a, c(b)))
}

# ============================================================================
# SCALAR + VECTOR operations (commutative)
# ============================================================================

# int + vector<int>
system_vector_add___int__vec_int <- function(a: int, b: vector<int>): vector<int> {
    return(system_vector_add___vec_int__int(b, a))
}

# double + vector<double>
system_vector_add___double__vec_double <- function(a: double, b: vector<double>): vector<double> {
    return(system_vector_add___vec_double__double(b, a))
}

# ============================================================================
# VECTOR - VECTOR operations (subtraction)
# ============================================================================

# vector<int> - vector<int>
system_vector_sub___vec_int__vec_int <- function(a: vector<int>, b: vector<int>): vector<int> {
    n <- length(a)
    m <- length(b)

    if(n != m & n %% m != 0 & m %% n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result: vector<int> <- vec(length=result_len, mode="int")

    for(i in 1L:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] - b[b_idx]
    }
    return(result)
}

# vector<double> - vector<double>
system_vector_sub___vec_double__vec_double <- function(a: vector<double>, b: vector<double>): vector<double> {
    n <- length(a)
    m <- length(b)

    if(n != m & n %% m != 0 & m %% n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result: vector<double> <- vec(length=result_len, mode="double")

    for(i in 1L:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] - b[b_idx]
    }
    return(result)
}

# ============================================================================
# VECTOR * VECTOR operations (multiplication)
# ============================================================================

# vector<int> * vector<int>
system_vector_mul___vec_int__vec_int <- function(a: vector<int>, b: vector<int>): vector<int> {
    n <- length(a)
    m <- length(b)

    if(n != m & n %% m != 0 & m %% n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result: vector<int> <- vec(length=result_len, mode="int")

    for(i in 1L:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] * b[b_idx]
    }
    return(result)
}

# vector<double> * vector<double>
system_vector_mul___vec_double__vec_double <- function(a: vector<double>, b: vector<double>): vector<double> {
    n <- length(a)
    m <- length(b)

    if(n != m & n %% m != 0 & m %% n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result: vector<double> <- vec(length=result_len, mode="double")

    for(i in 1L:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] * b[b_idx]
    }
    return(result)
}

# ============================================================================
# VECTOR / VECTOR operations (division)
# ============================================================================

# vector<int> / vector<int>
system_vector_div___vec_int__vec_int <- function(a: vector<int>, b: vector<int>): vector<int> {
    n <- length(a)
    m <- length(b)

    if(n != m & n %% m != 0 & m %% n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result: vector<int> <- vec(length=result_len, mode="int")

    for(i in 1L:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] / b[b_idx]
    }
    return(result)
}


# vector<double> / vector<double>
system_vector_div___vec_double__vec_double <- function(a: vector<double>, b: vector<double>): vector<double> {
    n <- length(a)
    m <- length(b)

    if(n != m & n %% m != 0 & m %% n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result: vector<double> <- vec(length=result_len, mode="double")

    for(i in 1L:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] / b[b_idx]
    }
    return(result)
}

# ============================================================================
# TYPE CASTING functions
# ============================================================================

# Cast vector<int> to vector<double>
system_cast_vec_int_to_vec_double <- function(vec: vector<int>): vector<double> {
    n <- length(vec)
    result: vector<double> <- vec(length=n, mode="double")

    for(i in 1L:n) {
        result[i] <- vec[i]
    }

    return(result)
}

# Cast vector<double> to vector<int>
system_cast_vec_double_to_vec_int <- function(vec: vector<double>): vector<int> {
    n <- length(vec)
    result: vector<int> <- vec(length=n, mode="int")

    for(i in 1L:n) {
        result[i] <- as.integer(vec[i])
    }

    return(result)
}

# ============================================================================
# COMPARISON operations (return vector<logical>)
# ============================================================================

# vector<int> < vector<int>
system_vector_less___vec_int__vec_int <- function(a: vector<int>, b: vector<int>): vector<logical> {
    n <- length(a)
    m <- length(b)

    if(n != m & n %% m != 0 & m %% n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result: vector<logical> <- vec(length=result_len, mode="logical")

    for(i in 1L:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] < b[b_idx]
    }
    return(result)
}

# vector<double> < vector<double>
system_vector_less___vec_double__vec_double <- function(a: vector<double>, b: vector<double>): vector<logical> {
    n <- length(a)
    m <- length(b)

    if(n != m & n %% m != 0 & m %% n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result: vector<logical> <- vec(length=result_len, mode="logical")

    for(i in 1L:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] < b[b_idx]
    }
    return(result)
}

# vector<int> <= vector<int>
system_vector_less_equal___vec_int__vec_int <- function(a: vector<int>, b: vector<int>): vector<logical> {
    n <- length(a)
    m <- length(b)

    if(n != m & n %% m != 0 & m %% n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result: vector<logical> <- vec(length=result_len, mode="logical")

    for(i in 1L:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] <= b[b_idx]
    }
    return(result)
}

# vector<double> <= vector<double>
system_vector_less_equal___vec_double__vec_double <- function(a: vector<double>, b: vector<double>): vector<logical> {
    n <- length(a)
    m <- length(b)

    if(n != m & n %% m != 0 & m %% n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result: vector<logical> <- vec(length=result_len, mode="logical")

    for(i in 1L:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] <= b[b_idx]
    }
    return(result)
}

# vector<int> > vector<int>
system_vector_greater___vec_int__vec_int <- function(a: vector<int>, b: vector<int>): vector<logical> {
    n <- length(a)
    m <- length(b)

    if(n != m & n %% m != 0 & m %% n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result: vector<logical> <- vec(length=result_len, mode="logical")

    for(i in 1L:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] > b[b_idx]
    }
    return(result)
}

# vector<double> > vector<double>
system_vector_greater___vec_double__vec_double <- function(a: vector<double>, b: vector<double>): vector<logical> {
    n <- length(a)
    m <- length(b)

    if(n != m & n %% m != 0 & m %% n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result: vector<logical> <- vec(length=result_len, mode="logical")

    for(i in 1L:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] > b[b_idx]
    }
    return(result)
}

# vector<int> >= vector<int>
system_vector_greater_equal___vec_int__vec_int <- function(a: vector<int>, b: vector<int>): vector<logical> {
    n <- length(a)
    m <- length(b)

    if(n != m & n %% m != 0 & m %% n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result: vector<logical> <- vec(length=result_len, mode="logical")

    for(i in 1L:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] >= b[b_idx]
    }
    return(result)
}

# vector<double> >= vector<double>
system_vector_greater_equal___vec_double__vec_double <- function(a: vector<double>, b: vector<double>): vector<logical> {
    n <- length(a)
    m <- length(b)

    if(n != m & n %% m != 0 & m %% n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result: vector<logical> <- vec(length=result_len, mode="logical")

    for(i in 1L:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] >= b[b_idx]
    }
    return(result)
}

# vector<int> == vector<int>
system_vector_equal___vec_int__vec_int <- function(a: vector<int>, b: vector<int>): vector<logical> {
    n <- length(a)
    m <- length(b)

    if(n != m & n %% m != 0 & m %% n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result: vector<logical> <- vec(length=result_len, mode="logical")

    for(i in 1L:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] == b[b_idx]
    }
    return(result)
}

# vector<double> == vector<double>
system_vector_equal___vec_double__vec_double <- function(a: vector<double>, b: vector<double>): vector<logical> {
    n <- length(a)
    m <- length(b)

    if(n != m & n %% m != 0 & m %% n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result: vector<logical> <- vec(length=result_len, mode="logical")

    for(i in 1L:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] == b[b_idx]
    }
    return(result)
}

# vector<int> != vector<int>
system_vector_not_equal___vec_int__vec_int <- function(a: vector<int>, b: vector<int>): vector<logical> {
    n <- length(a)
    m <- length(b)

    if(n != m & n %% m != 0 & m %% n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result: vector<logical> <- vec(length=result_len, mode="logical")

    for(i in 1L:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] != b[b_idx]
    }
    return(result)
}

# vector<double> != vector<double>
system_vector_not_equal___vec_double__vec_double <- function(a: vector<double>, b: vector<double>): vector<logical> {
    n <- length(a)
    m <- length(b)

    if(n != m & n %% m != 0 & m %% n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result: vector<logical> <- vec(length=result_len, mode="logical")

    for(i in 1L:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] != b[b_idx]
    }
    return(result)
}

# ============================================================================
# UTILITY functions
# ============================================================================

# ============================================================================
# VECTOR-SCALAR comparison operations (vector op scalar)
# ============================================================================

# vector<int> < int
system_vector_less___vec_int__int <- function(a: vector<int>, b: int): vector<logical> {
    return(system_vector_less___vec_int__vec_int(a, c(b)))
}

# vector<double> < double
system_vector_less___vec_double__double <- function(a: vector<double>, b: double): vector<logical> {
    return(system_vector_less___vec_double__vec_double(a, c(b)))
}

# vector<int> <= int
system_vector_less_equal___vec_int__int <- function(a: vector<int>, b: int): vector<logical> {
    return(system_vector_less_equal___vec_int__vec_int(a, c(b)))
}

# vector<double> <= double
system_vector_less_equal___vec_double__double <- function(a: vector<double>, b: double): vector<logical> {
    return(system_vector_less_equal___vec_double__vec_double(a, c(b)))
}

# vector<int> > int
system_vector_greater___vec_int__int <- function(a: vector<int>, b: int): vector<logical> {
    return(system_vector_greater___vec_int__vec_int(a, c(b)))
}

# vector<double> > double
system_vector_greater___vec_double__double <- function(a: vector<double>, b: double): vector<logical> {
    return(system_vector_greater___vec_double__vec_double(a, c(b)))
}

# vector<int> >= int
system_vector_greater_equal___vec_int__int <- function(a: vector<int>, b: int): vector<logical> {
    return(system_vector_greater_equal___vec_int__vec_int(a, c(b)))
}

# vector<double> >= double
system_vector_greater_equal___vec_double__double <- function(a: vector<double>, b: double): vector<logical> {
    return(system_vector_greater_equal___vec_double__vec_double(a, c(b)))
}

# vector<int> == int
system_vector_equal___vec_int__int <- function(a: vector<int>, b: int): vector<logical> {
    return(system_vector_equal___vec_int__vec_int(a, c(b)))
}

# vector<double> == double
system_vector_equal___vec_double__double <- function(a: vector<double>, b: double): vector<logical> {
    return(system_vector_equal___vec_double__vec_double(a, c(b)))
}

# vector<int> != int
system_vector_not_equal___vec_int__int <- function(a: vector<int>, b: int): vector<logical> {
    return(system_vector_not_equal___vec_int__vec_int(a, c(b)))
}

# vector<double> != double
system_vector_not_equal___vec_double__double <- function(a: vector<double>, b: double): vector<logical> {
    return(system_vector_not_equal___vec_double__vec_double(a, c(b)))
}

# ============================================================================
# SCALAR-VECTOR comparison operations (scalar op vector)
# ============================================================================

# int < vector<int>
system_vector_less___int__vec_int <- function(a: int, b: vector<int>): vector<logical> {
    return(system_vector_less___vec_int__vec_int(c(a), b))
}

# double < vector<double>
system_vector_less___double__vec_double <- function(a: double, b: vector<double>): vector<logical> {
    return(system_vector_less___vec_double__vec_double(c(a), b))
}

# int <= vector<int>
system_vector_less_equal___int__vec_int <- function(a: int, b: vector<int>): vector<logical> {
    return(system_vector_less_equal___vec_int__vec_int(c(a), b))
}

# double <= vector<double>
system_vector_less_equal___double__vec_double <- function(a: double, b: vector<double>): vector<logical> {
    return(system_vector_less_equal___vec_double__vec_double(c(a), b))
}

# int > vector<int>
system_vector_greater___int__vec_int <- function(a: int, b: vector<int>): vector<logical> {
    return(system_vector_greater___vec_int__vec_int(c(a), b))
}

# double > vector<double>
system_vector_greater___double__vec_double <- function(a: double, b: vector<double>): vector<logical> {
    return(system_vector_greater___vec_double__vec_double(c(a), b))
}

# int >= vector<int>
system_vector_greater_equal___int__vec_int <- function(a: int, b: vector<int>): vector<logical> {
    return(system_vector_greater_equal___vec_int__vec_int(c(a), b))
}

# double >= vector<double>
system_vector_greater_equal___double__vec_double <- function(a: double, b: vector<double>): vector<logical> {
    return(system_vector_greater_equal___vec_double__vec_double(c(a), b))
}

# int == vector<int>
system_vector_equal___int__vec_int <- function(a: int, b: vector<int>): vector<logical> {
    return(system_vector_equal___vec_int__vec_int(c(a), b))
}

# double == vector<double>
system_vector_equal___double__vec_double <- function(a: double, b: vector<double>): vector<logical> {
    return(system_vector_equal___vec_double__vec_double(c(a), b))
}

# int != vector<int>
system_vector_not_equal___int__vec_int <- function(a: int, b: vector<int>): vector<logical> {
    return(system_vector_not_equal___vec_int__vec_int(c(a), b))
}

# double != vector<double>
system_vector_not_equal___double__vec_double <- function(a: double, b: vector<double>): vector<logical> {
    return(system_vector_not_equal___vec_double__vec_double(c(a), b))
}

# Vector logical casting functions
system_cast_vec_logical_to_vec_int <- function(vec: vector<logical>): vector<int> {
    n <- length(vec)
    result: vector<int> <- vec(length=n, mode="int")
    for(i in 1L:n) {
        # FALSE -> 0, TRUE -> 1
        if(vec[i]) {
            result[i] <- 1L
        } else {
            result[i] <- 0L
        }
    }
    return(result)
}

system_cast_vec_logical_to_vec_double <- function(vec: vector<logical>): vector<double> {
    n <- length(vec)
    result: vector<double> <- vec(length=n, mode="double")
    for(i in 1L:n) {
        # FALSE -> 0.0, TRUE -> 1.0
        if(vec[i]) {
            result[i] <- 1.0
        } else {
            result[i] <- 0.0
        }
    }
    return(result)
}

system_cast_vec_int_to_vec_logical <- function(vec: vector<int>): vector<logical> {
    n <- length(vec)
    result: vector<logical> <- vec(length=n, mode="logical")
    for(i in 1L:n) {
        # 0 -> FALSE, non-zero -> TRUE
        result[i] <- vec[i] != 0
    }
    return(result)
}

system_cast_vec_double_to_vec_logical <- function(vec: vector<double>): vector<logical> {
    n <- length(vec)
    result: vector<logical> <- vec(length=n, mode="logical")
    for(i in 1L:n) {
        # 0.0 -> FALSE, non-zero -> TRUE
        result[i] <- vec[i] != 0.0
    }
    return(result)
}
