# Statistical functions for vectors
# Naming scheme: system_name___arg1__arg2__arg3
#   - THREE underscores (___) between function name and first arg
#   - TWO underscores (__) between arguments

# ============================================================================
# MEAN - Calculate arithmetic mean
# ============================================================================

# mean(vector<int>) -> double
system_mean___vec_int <- function(x: vector<int>): double {
    n <- length(x)
    if(n == 0L) {
        stop("Cannot compute mean of empty vector")
    }

    sum <- 0.0
    i <- 1L
    while(i <= n) {
        sum <- sum + x[i]
        i <- i + 1L
    }
    return(sum / n)
}

# mean(vector<double>) -> double
system_mean___vec_double <- function(x: vector<double>): double {
    n <- length(x)
    if(n == 0) {
        stop("Cannot compute mean of empty vector")
    }

    sum <- 0.0
    i <- 1
    while(i <= n) {
        sum <- sum + x[i]
        i <- i + 1
    }
    return(sum / n)
}

# ============================================================================
# MEDIAN - Calculate median (middle value when sorted)
# ============================================================================

# Helper function to swap elements (needed for sorting)
swap_int <- function(vec: vector<int>, i: double, j: double): void {
    temp <- vec[i]
    vec[i] <- vec[j]
    vec[j] <- temp
    return()
}

swap_double <- function(vec: vector<double>, i: double, j: double): void {
    temp <- vec[i]
    vec[i] <- vec[j]
    vec[j] <- temp
    return()
}

# median(vector<int>) -> double
system_median___vec_int <- function(x: vector<int>): double {
    n <- length(x)
    if(n == 0L) {
        stop("Cannot compute median of empty vector")
    }

    # Create a copy and sort it
    sorted: vector<int> <- vec(length=n, mode="int")
    i <- 1L
    while(i <= n) {
        sorted[i] <- x[i]
        i <- i + 1L
    }

    # Simple bubble sort
    i <- 1L
    while(i <= n - 1L) {
        j <- 1L
        while(j <= n - i) {
            if(sorted[j] > sorted[j+1L]) {
                swap_int(sorted, j, j+1L)
            }
            j <- j + 1L
        }
        i <- i + 1L
    }

    # Calculate median
    if(n %% 2L == 1L) {
        # Odd: return middle element
        mid <- (n + 1L) / 2L
        return(sorted[mid])
    } else {
        # Even: return average of two middle elements
        mid1 <- n / 2L
        mid2 <- mid1 + 1L
        return((sorted[mid1] + sorted[mid2]) / 2.0)
    }
}

# median(vector<double>) -> double
system_median___vec_double <- function(x: vector<double>): double {
    n <- length(x)
    if(n == 0) {
        stop("Cannot compute median of empty vector")
    }

    # Create a copy and sort it
    sorted: vector<double> <- vec(length=n, mode="double")
    i <- 1
    while(i <= n) {
        sorted[i] <- x[i]
        i <- i + 1
    }

    # Simple bubble sort
    i <- 1
    while(i <= n - 1) {
        j <- 1
        while(j <= n - i) {
            if(sorted[j] > sorted[j+1]) {
                swap_double(sorted, j, j+1)
            }
            j <- j + 1
        }
        i <- i + 1
    }

    # Calculate median
    if(n %% 2 == 1) {
        # Odd: return middle element
        mid <- (n + 1) / 2
        return(sorted[mid])
    } else {
        # Even: return average of two middle elements
        mid1 <- n / 2
        mid2 <- mid1 + 1
        return((sorted[mid1] + sorted[mid2]) / 2.0)
    }
}

# ============================================================================
# VARIANCE - Calculate sample variance
# ============================================================================

# var(vector<int>) -> double
system_var___vec_int <- function(x: vector<int>): double {
    n <- length(x)
    if(n <= 1L) {
        stop("Cannot compute variance with less than 2 elements")
    }

    mean_val <- system_mean___vec_int(x)
    sum_sq_diff <- 0.0

    i <- 1L
    while(i <= n) {
        diff <- x[i] - mean_val
        sum_sq_diff <- sum_sq_diff + (diff * diff)
        i <- i + 1L
    }

    return(sum_sq_diff / (n - 1L))
}

# var(vector<double>) -> double
system_var___vec_double <- function(x: vector<double>): double {
    n <- length(x)
    if(n <= 1) {
        stop("Cannot compute variance with less than 2 elements")
    }

    mean_val <- system_mean___vec_double(x)
    sum_sq_diff <- 0.0

    i <- 1
    while(i <= n) {
        diff <- x[i] - mean_val
        sum_sq_diff <- sum_sq_diff + (diff * diff)
        i <- i + 1
    }

    return(sum_sq_diff / (n - 1))
}

# ============================================================================
# STANDARD DEVIATION - Calculate sample standard deviation
# ============================================================================

# sd(vector<int>) -> double
system_sd___vec_int <- function(x: vector<int>): double {
    variance <- system_var___vec_int(x)
    return(sqrt(variance))
}

# sd(vector<double>) -> double
system_sd___vec_double <- function(x: vector<double>): double {
    variance <- system_var___vec_double(x)
    return(sqrt(variance))
}

# ============================================================================
# QUANTILE - Calculate quantiles (simplified version)
# ============================================================================

# quantile(vector<int>, double) -> double
# Calculates a specific quantile (0 <= prob <= 1)
system_quantile___vec_int__double <- function(x: vector<int>, prob: double): double {
    n <- length(x)
    if(n == 0L) {
        stop("Cannot compute quantile of empty vector")
    }

    if(prob < 0.0 | prob > 1.0) {
        stop("Probability must be between 0 and 1")
    }

    # Create a copy and sort it
    sorted: vector<int> <- vec(length=n, mode="int")
    i <- 1L
    while(i <= n) {
        sorted[i] <- x[i]
        i <- i + 1L
    }

    # Simple bubble sort
    i <- 1L
    while(i <= n - 1L) {
        j <- 1L
        while(j <= n - i) {
            if(sorted[j] > sorted[j+1L]) {
                swap_int(sorted, j, j+1L)
            }
            j <- j + 1L
        }
        i <- i + 1L
    }

    # Calculate position (using type 7 quantile algorithm, similar to R default)
    h <- (n - 1L) * prob + 1.0
    h_floor <- as.integer(h)

    if(h_floor >= n) {
        return(sorted[n])
    }

    if(h_floor < 1L) {
        return(sorted[1L])
    }

    # Linear interpolation
    lower <- sorted[h_floor]
    upper <- sorted[h_floor + 1L]
    fraction <- h - h_floor

    return(lower + (upper - lower) * fraction)
}

# quantile(vector<double>, double) -> double
system_quantile___vec_double__double <- function(x: vector<double>, prob: double): double {
    n <- length(x)
    if(n == 0) {
        stop("Cannot compute quantile of empty vector")
    }

    if(prob < 0.0 | prob > 1.0) {
        stop("Probability must be between 0 and 1")
    }

    # Create a copy and sort it
    sorted: vector<double> <- vec(length=n, mode="double")
    i <- 1
    while(i <= n) {
        sorted[i] <- x[i]
        i <- i + 1
    }

    # Simple bubble sort
    i <- 1
    while(i <= n - 1) {
        j <- 1
        while(j <= n - i) {
            if(sorted[j] > sorted[j+1]) {
                swap_double(sorted, j, j+1)
            }
            j <- j + 1
        }
        i <- i + 1
    }

    # Calculate position
    h <- (n - 1) * prob + 1.0
    h_floor <- as.integer(h)

    if(h_floor >= n) {
        return(sorted[n])
    }

    if(h_floor < 1) {
        return(sorted[1])
    }

    # Linear interpolation
    lower <- sorted[h_floor]
    upper <- sorted[h_floor + 1]
    fraction <- h - h_floor

    return(lower + (upper - lower) * fraction)
}

# ============================================================================
# RANGE - Return min and max as a vector
# ============================================================================

# range(vector<int>) -> vector<int>
system_range___vec_int <- function(x: vector<int>): vector<int> {
    n <- length(x)
    if(n == 0L) {
        stop("Cannot compute range of empty vector")
    }

    min_val <- system_min___vec_int(x)
    max_val <- system_max___vec_int(x)

    result: vector<int> <- vec(length=2L, mode="int")
    result[1L] <- min_val
    result[2L] <- max_val

    return(result)
}

# range(vector<double>) -> vector<double>
system_range___vec_double <- function(x: vector<double>): vector<double> {
    n <- length(x)
    if(n == 0) {
        stop("Cannot compute range of empty vector")
    }

    min_val <- system_min___vec_double(x)
    max_val <- system_max___vec_double(x)

    result: vector<double> <- vec(length=2, mode="double")
    result[1] <- min_val
    result[2] <- max_val

    return(result)
}
