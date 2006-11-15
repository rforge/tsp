## create a (asymmetric) ATSP problem
ATSP <- function(x) {
    if(inherits(x, "ATSP")) return(x)
    
    method <- attr(x, "method")
    
    ## check x
    if(inherits(x, "dist")) {
        x <- as.matrix(x)
    }else if(is.matrix(x) && dim(x)[1] == dim(x)[2]) {
        ## matrix is ok
    }else stop("ATSP requires a square matrix")

    ## check for NAs
    if(any(is.nan(x))) stop(paste(sQuote("NAs"), "not supported"))
    
    class(x) <- c("ATSP", class(x))
    attr(x, "method") <- method
    x
}

## print
print.ATSP <- function(x, ...) {
    method <- attr(x, "method")
    if(is.null(method)) method <- "unknown"
    
    cat("object of class", sQuote(class(x)[1]), " (asymmetric TSP) \n")
    cat(n_of_cities(x), "cities", 
        paste("(distance ", sQuote(method),")", sep=""), "\n")
}


## number of cities
n_of_cities.ATSP <- function(x) nrow(x)

## labels
labels.ATSP <- function(object, ...) dimnames(object)[[1]]

## image
image.ATSP <- function(x, order, col = gray.colors(64), ...) {
    p <- n_of_cities(x)
    if(missing(order)) order <- 1:p
    
    image.default(1:p, 1:p, x[order, order], col = col, ...)
}

## coerce to matrix
as.matrix.ATSP <- function(x){
    unclass(x)
}
