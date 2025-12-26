add_vec <- function(): vector<double> {
    x <- c(1, 2, 3)
    y <- c(4, 5, 6)

    my_vector <- x + y
    return(my_vector)
}

print(sum(add_vec()))