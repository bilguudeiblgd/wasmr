my_generator <- function(s: int, e: int, step: int): () -> int {
    current <- s
    function(): int {
        if (current + step < e){
            current <<- current + step
            return(current)
        }
        else {
            return(-1)
        }
    }
}

ma <- my_generator(1, 10, 1)
print(ma())
print(ma())
print(ma())
print(ma())
