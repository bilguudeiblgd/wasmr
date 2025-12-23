# Generic vector operations for all numeric types
# Function names follow the mangling scheme: base___type1__type2
# where types are: vec_int, vec_float, vec_double, int, float, double

# ============================================================================
# VECTOR + VECTOR operations
# ============================================================================

# vector<int> + vector<int>
system_vector_add___vec_int__vec_int <- function(a: vector<int>, b: vector<int>): vector<int> {
    n <- length(a)
    m <- length(b)
    higher <- a
    lower <- b
    higher_size <- n
    lower_size <- m

    if(m > n) {
        higher <- b
        lower <- a
        higher_size <- m
        lower_size <- n
    }

    if(higher_size % lower_size != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result <- higher
    for(i in 1:higher_size) {
        lower_idx <- ((i - 1) % lower_size) + 1
        result[i] <- higher[i] + lower[lower_idx]
    }
    return(result)
}

# vector<float> + vector<float>
system_vector_add___vec_float__vec_float <- function(a: vector<float>, b: vector<float>): vector<float> {
    n <- length(a)
    m <- length(b)
    higher <- a
    lower <- b
    higher_size <- n
    lower_size <- m

    if(m > n) {
        higher <- b
        lower <- a
        higher_size <- m
        lower_size <- n
    }

    if(higher_size % lower_size != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result <- higher
    for(i in 1:higher_size) {
        lower_idx <- ((i - 1) % lower_size) + 1
        result[i] <- higher[i] + lower[lower_idx]
    }
    return(result)
}

# vector<double> + vector<double>
system_vector_add___vec_double__vec_double <- function(a: vector<double>, b: vector<double>): vector<double> {
    n <- length(a)
    m <- length(b)
    higher <- a
    lower <- b
    higher_size <- n
    lower_size <- m

    if(m > n) {
        higher <- b
        lower <- a
        higher_size <- m
        lower_size <- n
    }

    if(higher_size % lower_size != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result <- higher
    for(i in 1:higher_size) {
        lower_idx <- ((i - 1) % lower_size) + 1
        result[i] <- higher[i] + lower[lower_idx]
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

# vector<float> + float
system_vector_add___vec_float__float <- function(a: vector<float>, b: float): vector<float> {
    return(system_vector_add___vec_float__vec_float(a, c(b)))
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

# float + vector<float>
system_vector_add___float__vec_float <- function(a: float, b: vector<float>): vector<float> {
    return(system_vector_add___vec_float__float(b, a))
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

    if(n != m && n % m != 0 && m % n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result <- c()

    for(i in 1:result_len) {
        a_idx <- ((i - 1) % n) + 1
        b_idx <- ((i - 1) % m) + 1
        result[i] <- a[a_idx] - b[b_idx]
    }
    return(result)
}

# vector<float> - vector<float>
system_vector_sub___vec_float__vec_float <- function(a: vector<float>, b: vector<float>): vector<float> {
    n <- length(a)
    m <- length(b)

    if(n != m && n % m != 0 && m % n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result <- c()

    for(i in 1:result_len) {
        a_idx <- ((i - 1) % n) + 1
        b_idx <- ((i - 1) % m) + 1
        result[i] <- a[a_idx] - b[b_idx]
    }
    return(result)
}

# vector<double> - vector<double>
system_vector_sub___vec_double__vec_double <- function(a: vector<double>, b: vector<double>): vector<double> {
    n <- length(a)
    m <- length(b)

    if(n != m && n % m != 0 && m % n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result <- c()

    for(i in 1:result_len) {
        a_idx <- ((i - 1) % n) + 1
        b_idx <- ((i - 1) % m) + 1
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
    higher <- a
    lower <- b
    higher_size <- n
    lower_size <- m

    if(m > n) {
        higher <- b
        lower <- a
        higher_size <- m
        lower_size <- n
    }

    if(higher_size % lower_size != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result <- higher
    for(i in 1:higher_size) {
        lower_idx <- ((i - 1) % lower_size) + 1
        result[i] <- higher[i] * lower[lower_idx]
    }
    return(result)
}

# vector<float> * vector<float>
system_vector_mul___vec_float__vec_float <- function(a: vector<float>, b: vector<float>): vector<float> {
    n <- length(a)
    m <- length(b)
    higher <- a
    lower <- b
    higher_size <- n
    lower_size <- m

    if(m > n) {
        higher <- b
        lower <- a
        higher_size <- m
        lower_size <- n
    }

    if(higher_size % lower_size != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result <- higher
    for(i in 1:higher_size) {
        lower_idx <- ((i - 1) % lower_size) + 1
        result[i] <- higher[i] * lower[lower_idx]
    }
    return(result)
}

# vector<double> * vector<double>
system_vector_mul___vec_double__vec_double <- function(a: vector<double>, b: vector<double>): vector<double> {
    n <- length(a)
    m <- length(b)
    higher <- a
    lower <- b
    higher_size <- n
    lower_size <- m

    if(m > n) {
        higher <- b
        lower <- a
        higher_size <- m
        lower_size <- n
    }

    if(higher_size % lower_size != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result <- higher
    for(i in 1:higher_size) {
        lower_idx <- ((i - 1) % lower_size) + 1
        result[i] <- higher[i] * lower[lower_idx]
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

    if(n != m && n % m != 0 && m % n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result <- c()

    for(i in 1:result_len) {
        a_idx <- ((i - 1) % n) + 1
        b_idx <- ((i - 1) % m) + 1
        result[i] <- a[a_idx] / b[b_idx]
    }
    return(result)
}

# vector<float> / vector<float>
system_vector_div___vec_float__vec_float <- function(a: vector<float>, b: vector<float>): vector<float> {
    n <- length(a)
    m <- length(b)

    if(n != m && n % m != 0 && m % n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result <- c()

    for(i in 1:result_len) {
        a_idx <- ((i - 1) % n) + 1
        b_idx <- ((i - 1) % m) + 1
        result[i] <- a[a_idx] / b[b_idx]
    }
    return(result)
}

# vector<double> / vector<double>
system_vector_div___vec_double__vec_double <- function(a: vector<double>, b: vector<double>): vector<double> {
    n <- length(a)
    m <- length(b)

    if(n != m && n % m != 0 && m % n != 0) {
        stop("Vector lengths not compatible for recycling")
    }

    result_len <- max(n, m)
    result <- c()

    for(i in 1:result_len) {
        a_idx <- ((i - 1) % n) + 1
        b_idx <- ((i - 1) % m) + 1
        result[i] <- a[a_idx] / b[b_idx]
    }
    return(result)
}

# ============================================================================
# UTILITY functions
# ============================================================================

# Sum of vector<int>
system_vector_sum___vec_int <- function(a: vector<int>): int {
    sum <- 0
    for(i in a) {
        sum <- sum + i
    }
    return(sum)
}

# Sum of vector<float>
system_vector_sum___vec_float <- function(a: vector<float>): float {
    sum <- 0.0
    for(i in a) {
        sum <- sum + i
    }
    return(sum)
}

# Sum of vector<double>
system_vector_sum___vec_double <- function(a: vector<double>): double {
    sum <- 0.0
    for(i in a) {
        sum <- sum + i
    }
    return(sum)
}
