my_generator <- function(s: int, e: int, step: int): () -> int {
    current <- s
    function() {
        if (current + step < e){
            current <<- current + step
            return(current)
        }
        else {
            return(-1)
        }

    }
}

m <- my_generator(1, 10, 1)
print(m())
print(m())
print(m())
print(m())
