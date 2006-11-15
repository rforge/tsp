TOUR <- function(x){
    if(inherits(x, "TOUR")) return(x)
    
    x <- as.integer(x)
    class(x) <- "TOUR"
    x
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
