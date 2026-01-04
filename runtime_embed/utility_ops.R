
system_seq___int___int___int <- function(from: int = 1L, to: int = 1L, by: int = 1L): vector<int> {
    res: vector<int> <- vec(length = to - from + 1, mode = "int")
    i <- 1
    while(from <= to) {
        res[i] <- from
        i <- i + 1
        from <- from + 1L
    }
    return(res)
}

system_seq___double___double___double <- function(from: double = 1, to: double = 1, by: double = 1): vector<double> {
    res: vector<double> <- vec(length = to - from + 1, mode = "double")
    i <- 1
    while(from <= to) {
        res[i] <- from
        i <- i + 1
        from <- from + 1
    }
    return(res)
}

# Sum of vector<int>
system_sum___vec_int <- function(a: vector<int>): int {
    sum : int <- 0L
    for(i in a) {
        sum <- sum + i
    }
    return(sum)
}


# Sum of vector<logical>
system_sum___vec_logical <- function(a: vector<logical>): int {
    sum <- 0L
    for(i in a) {
        sum <- sum + i
    }
    return(sum)
}


# Sum of vector<double>
system_sum___vec_double <- function(a: vector<double>): double {
    sum <- 0.0
    for(i in a) {
        sum <- sum + i
    }
    return(sum)
}


# Rep for vector<int>
system_rep___vec_int__int__int <- function(x: vector<int>, times: int = 1L, each: int = 1L): vector<int> {
    len <- length(x)
    new_len <- len * each * times
    res: vector<int> <- vec(length = new_len, mode = "int")

    idx <- 1
    t <- 1
    while(t <= times) {
        i <- 1
        while(i <= len) {
            e <- 1
            while(e <= each) {
                res[idx] <- x[i]
                idx <- idx + 1
                e <- e + 1
            }
            i <- i + 1
        }
        t <- t + 1
    }

    return(res)
}


# Rep for vector<double>
system_rep___vec_double__int__int <- function(x: vector<double>, times: int = 1L, each: int = 1L): vector<double> {
    len <- length(x)
    new_len <- len * each * times
    res: vector<double> <- vec(length = new_len, mode = "double")

    idx <- 1
    t <- 1
    while(t <= times) {
        i <- 1
        while(i <= len) {
            e <- 1
            while(e <= each) {
                res[idx] <- x[i]
                idx <- idx + 1
                e <- e + 1
            }
            i <- i + 1
        }
        t <- t + 1
    }

    return(res)
}


# Rep for vector<logical>
system_rep___vec_logical__int__int <- function(x: vector<logical>, times: int = 1L, each: int = 1L): vector<logical> {
    len <- length(x)
    new_len <- len * each * times
    res: vector<logical> <- vec(length = new_len, mode = "logical")

    idx <- 1
    t <- 1
    while(t <= times) {
        i <- 1
        while(i <= len) {
            e <- 1
            while(e <= each) {
                res[idx] <- x[i]
                idx <- idx + 1
                e <- e + 1
            }
            i <- i + 1
        }
        t <- t + 1
    }

    return(res)
}


# Rep for scalar int
system_rep___int__int__int <- function(x: int, times: int = 1L, each: int = 1L): vector<int> {
    new_len <- each * times
    res: vector<int> <- vec(length = new_len, mode = "int")

    idx <- 1
    while(idx <= new_len) {
        res[idx] <- x
        idx <- idx + 1
    }

    return(res)
}


# Rep for scalar double
system_rep___double__int__int <- function(x: double, times: int = 1L, each: int = 1L): vector<double> {
    new_len <- each * times
    res: vector<double> <- vec(length = new_len, mode = "double")

    idx <- 1
    while(idx <= new_len) {
        res[idx] <- x
        idx <- idx + 1
    }

    return(res)
}


# Rep for scalar logical
system_rep___logical__int__int <- function(x: logical, times: int = 1L, each: int = 1L): vector<logical> {
    new_len <- each * times
    res: vector<logical> <- vec(length = new_len, mode = "logical")

    idx <- 1
    while(idx <= new_len) {
        res[idx] <- x
        idx <- idx + 1
    }

    return(res)
}
