basic_loop <- function(): double {
    sum <- 0
    for(i in 1:5) {
        sum <- sum + i
    }
    return(sum)
}

sum <- basic_loop()
print(sum)