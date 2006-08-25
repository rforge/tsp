# create a TSP problem
TSP <- function(x) {
    if(inherits(x, "tsp")) return(x)
    
    # check x
    if(inherits(x, "dist")) {
        # make sure we have a upper triangle matrix w/o diagonal
        if(attr(x, "Diag") == TRUE || attr(x, "Upper") == TRUE)
        x <- as.dist(x, diag = FALSE, upper = FALSE)
    }else if(is.matrix(x)) {
        if(!isSymmetric(x)) stop(paste(sQuote("x"), "is not symmetric"))
    }else stop("the symmetric TSP requires a object of class", 
        sQuote("dist"), "or a symmetric matrix")
    
    class(x) <- c("TSP", class(x))
    x
}

# print
print.TSP <- function(x, ...) {
    cat("object of class", sQuote(class(x)[1]), "\n")
    cat("problem stored as object of class", sQuote(class(x)[2]),
        "with", cities(x), "cities","\n")
}


# number of cities
cities.TSP <- function(x) {
    if(inherits(x, "dist")) return(attr(x, "Size"))
    if(inherits(x, "matrix")) return(nrow(x))
    stop("unknown storage format")
}

# generic for cities
cities <- function(x) UseMethod("cities")
cities.default <- cities.TSP

# labels
labels.TSP <- function(object, ...) {
    if(inherits(object, "dist")) return(attr(object, "Labels"))
    if(inherits(object, "matrix")) return(dimnames(object)[[1]])
}

# image
image.TSP <- function(x, order, col = gray.colors(64), ...) {
    p <- cities(x)
    if(missing(order)) order <- 1:p
    
    image.default(1:p, 1:p, as.matrix(x)[order, order], col = col, ...)
}

