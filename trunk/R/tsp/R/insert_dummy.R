# insert a dummy city

# TODO: if n > 1 the distances between the cities have to be inf...

insert_dummy.dist <- function(x, n = 1, c = 1) {

    n <- as.integer(n)
    p <- attr(x, "Size")

    d <- c(rep(c, sum(p:(p+n-1))), x)
    
    attributes(d) <- attributes(x)
    attr(d, "Size") <- p + n
    attr(d, "Labels") <- c(rep("dummy", n), attr(x, "Labels"))
    d
}

insert_dummy.matrix <- function(x, n = 1, c = 1) {
   
    n <- as.integer(n)
    x <- cbind(matrix(c, ncol = n, nrow = nrow(x), 
            dimnames = list(rows = NULL, cols = rep("dummy", n))), x)
    x <- rbind(matrix(c, ncol = ncol(x), nrow = n,
            dimnames = list(rows = rep("dummy", n), cols = NULL)), x)
        
    x[1:n,1:n] <- 0

    x
}

# generic
insert_dummy <- function(x, n = 1, c = 1) UseMethod("insert_dummy")
insert_dummy.default <- insert_dummy.matrix

