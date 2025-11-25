basic_loop <- function(): int {
    sum <- 0
    for(i in 1:5) {
        sum <- sum + i
    }
    print(sum)
    return(sum)
}

basic_loop()