# Greedy tour also called nearest neighbor algorithm

tsp_greedy <- function(x, start = 0) {

    # check parameters
    if(!inherits(x, "dist") || (is.matrix(x) && isSymmetric(x)))
    stop(paste(sQuote("x"), "is not of class", sQuote("dist"),
            "or a symmetric matrix."))

    x <- as.matrix(x)
    n <- nrow(x)
    if(start == 0) start <- sample(1:n, 1)
    
    # place first city
    placed <- logical(n)
    placed[start] <- TRUE
    current <- start
    order <- c(start)

    while(any(placed == FALSE)) {
        # find nearest city
        rest <- which(!placed)
        # nearest <- rest[which.min(x[current,rest])]
        # which.min has problems with Inf
        # so we can break ties randomly now too
        x_sub <- x[current,rest]
        nearest <- rest[which(x_sub == min(x_sub))]
        if(length(nearest) > 1) nearest <- sample(nearest, 1)
        placed[nearest] <- TRUE
        current <- nearest
        order <- c(order, nearest)
    }


   order 
}
