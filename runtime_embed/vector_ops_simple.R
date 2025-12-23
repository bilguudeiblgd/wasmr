# Simplified version for testing

# vector<int> + vector<int>
system_vector_add___vec_int__vec_int <- function(a: vector<int>, b: vector<int>): vector<int> {
    n <- length(a)
    result <- a

    for(i in 1:n) {
        result[i] <- a[i] + b[i]
    }

    return(result)
}
