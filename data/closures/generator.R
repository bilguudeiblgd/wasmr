my_generator <- function(s: int, e: int, step: int) {
    current <- s
    function() {
        if (current + step < e){
            current <<- current + step
            return(current)
        }
        else {
            return(NULL)
        }

    }
}

m <- my_generator(1, 10, 1)
m()
m()
m()
m()