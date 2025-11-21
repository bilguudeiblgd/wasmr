
system_vector_add <- function(a: vector<int>, b: vector<int>): vector<int> {
    n = length(a)
    m = length(b)
    higher = a
    lower = b
    higher_size = n
    lower_size = m
    if(m > n) {
        higher = b
        lower = a
        higher_size = m
        lower_size = n
    }
    if(higher_size % lower_size != 0) {
        stop("not possible.")
    }

    result = higher
    for(i in 1..higher_size) {
        result[i] = higher[i] + lower[i % lower_size]
    }
    
}