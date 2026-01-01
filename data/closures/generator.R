my_generator <- function(s: int, e: int, step: int): () -> int {
    current <- s
    function(): int {
        if (current + step < e){
            current <<- current + step
            return(current)
        }
        else {
            return(-1L)
        }
    }
}

ma <- my_generator(1L, 10L, 1L)
print(ma())
print(ma())
print(ma())
print(ma())
