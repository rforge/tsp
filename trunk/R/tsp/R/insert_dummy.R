# insert a dummy city

# TODO: if n > 1 the distances between the cities have to be inf...

insert_dummy.dist <- function(x, n = 1, const = max(x), inf = Inf) {

    if(n < 1) stop(paste(sQuote("n"),"has to be >1"))
    
    if(attr(x, "Diag") == TRUE || attr(x, "Upper") == TRUE)
    x <- as.dist(x, diag = FALSE, upper = FALSE)
    
    n <- as.integer(n)
    p <- attr(x, "Size")

    d <- c(rep(const, sum(p:(p+n-1))), x)
    p <- p + n
    
    # place inf between dummies
    # access a dist object for i < j <= p is
    # dist[p*(i-1) - i*(i-1)/2 + j-i]
    if(n>1) {
        for(i in 1:(n-1)) {
            for(j in (i+1):n) {
                d[p*(i-1) - i*(i-1)/2 + j-i] <- inf
            }
        }
    }    
    
    attributes(d) <- attributes(x)
    attr(d, "Size") <- p
    attr(d, "Labels") <- c(paste("dummy", 1:n, sep = ""), attr(x, "Labels"))
    d
}

insert_dummy.matrix <- function(x, n = 1, const = max(x), inf = Inf) {
   
    n <- as.integer(n)
    x <- cbind(matrix(const, ncol = n, nrow = nrow(x), 
            dimnames = list(rows = NULL, 
                cols = paste("dummy", 1:n, sep=""))), x)
    x <- rbind(matrix(const, ncol = ncol(x), nrow = n,
            dimnames = list(rows = paste("dummy", 1:n, sep=""), 
                cols = NULL)), x)
    
    x[1:n,1:n] <- 0

    if(n>1) {
        for(i in 1:(n-1)) {
            for(j in (i+1):n) {
                x[i,j] <- Inf
                x[j,i] <- Inf
            }
        }
    }    
    
    x
}

# generic
insert_dummy <- function(x, n = 1, const = max(x), inf = Inf) 
UseMethod("insert_dummy")
insert_dummy.default <- insert_dummy.matrix

