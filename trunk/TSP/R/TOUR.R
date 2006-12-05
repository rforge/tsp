TOUR <- function(x){
    if(inherits(x, "TOUR")) return(x)
    
    as.TOUR(x)
}

## coercion
as.TOUR <- function(object) UseMethod("as.TOUR")
as.TOUR.integer <- function(object){
    object <- as.integer(object)
    
    ## check tour
    if(any(object < 1 || object > length(object) || is.na(object))) 
        stop("tour contains illegal elements.")
    
    class(object) <- "TOUR"
    object
}



## helper for tour

print.TOUR <- function(x, ...){

   cat("object of class", sQuote(class(x)[1]), "\n")
   cat("result of method", sQuote(attr(x, "method")), "for", 
       length(x), "cities\n")
   if(!is.null(attr(x, "tour_length")))
   cat("tour length:", attr(x, "tour_length"), "\n")
   else 
   cat("tour length: unknown\n")
}
