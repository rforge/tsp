

tour_length <- function(dist, order) {
   
    # check arguments
    if (!inherits(dist, "dist"))
    stop(paste(sQuote("dist"),"not of class dist"))
    
    if(attr(dist, "Diag") == TRUE || attr(dist, "Upper") == TRUE)
    dist <- as.dist(dist, diag = FALSE, upper = FALSE)
    
    n <- attr(dist, "Size")
    if(missing(order)) order <- 1:n

    # calculate the tour length
    # access a dist object for i < j <= n is
    # dist[n*(i-1) - i*(i-1)/2 + j-i]
    s <- sapply(1:(n-1), FUN = function(k) {
            ij <- order[k:(k+1)]
            i <- min(ij)
            j <- max(ij)
            dist[n*(i-1) - i*(i-1)/2 + j-i]
        })

    # now we close the tour
    ij <- order[c(1,n)]
    i <- min(ij)
    j <- max(ij)
    sum(s) + dist[n*(i-1) - i*(i-1)/2 + j-i]
}
