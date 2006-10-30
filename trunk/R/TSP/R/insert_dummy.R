## insert a dummy city

## TODO: if n > 1 the distances between the cities have to be inf...

insert_dummy.TSP <- function(x, n = 1, const = 0, inf = Inf) {
    
    if(n < 1) stop(paste(sQuote("n"),"has to be >1"))
    
    n <- as.integer(n)
    p <- n_of_cities(x)

    d <- c(rep(const, sum(p:(p+n-1))), x)
    p <- p + n
    
    ## place inf between dummies
    ## access a dist object for i < j <= p is
    ## dist[p*(i-1) - i*(i-1)/2 + j-i]
    if(n>1) {
        for(i in 1:(n-1)) {
            for(j in (i+1):n) {
                d[p*(i-1) - i*(i-1)/2 + j-i] <- inf
            }
        }
    }    
    
    attributes(d) <- attributes(x)
    attr(d, "Size") <- p
    attr(d, "Labels") <- c(rep("dummy", n), attr(x, "Labels"))
    d
}

insert_dummy.ATSP <- function(x, n = 1, const = 0, inf = Inf) {
    
    n <- as.integer(n)
    p <- n_of_cities(x)
    
    ## add dummy rows/columns
    x <- cbind(matrix(0, ncol = n, nrow = p, 
            dimnames = list(NULL, rep("dummy", n))), x)
    x <- rbind(matrix(0, ncol = p+n, nrow = n, 
            dimnames = list(rep("dummy", n), NULL)), x)

    ## place inf between dummies
    if(n>1) {
        for(i in 1:(n-1)) {
            for(j in (i+1):n) {
                x[i, j] <- x[j, i] <- inf
            }
        }
    }    
    
    x
}

##generic
insert_dummy <- function(x, n = 1, const = 0, inf = Inf) 
    UseMethod("insert_dummy")

