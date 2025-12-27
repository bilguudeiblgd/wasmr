
system_seq___int___int___int <- function(from: int = 1, to: int = 1, by: int = 1): vector<int> {
    res: vector<int> <- vec(length = to - from + 1, mode = "int")
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
    sum <- 0
    for(i in a) {
        sum <- sum + i
    }
    return(sum)
}


# Sum of vector<logical>
system_sum___vec_logical <- function(a: vector<logical>): int {
    sum <- 0
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
