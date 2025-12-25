
system_seq___int___int___int <- function(from: int = 1, to: int = 1, by: int = 1): vector<int> {
    res : vector<int> <- vec(length = to - from)
    i <- 0
    while(from <= to) {
        res[i] <- from
        i <- i + 1
        from <- from + 1
    }
    res
}