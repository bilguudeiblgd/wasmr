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

    for(i in 1:result_len) {
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

    for(i in 1:result_len) {
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

    for(i in 1:result_len) {
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

    for(i in 1:result_len) {
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

    for(i in 1:result_len) {
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

    for(i in 1:result_len) {
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

    for(i in 1:result_len) {
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

    for(i in 1:result_len) {
        a_idx <- ((i - 1) %% n) + 1
        b_idx <- ((i - 1) %% m) + 1
        result[i] <- a[a_idx] / b[b_idx]
    }
    return(result)
}

# ============================================================================
# UTILITY functions
# ============================================================================
