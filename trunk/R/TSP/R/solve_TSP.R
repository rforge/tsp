## TSP
solve_TSP.TSP <- function(x, method = NULL, control = NULL) {
    .solve_TSP(x, method, control)
}

## ATSP
solve_TSP.ATSP <- function(x, method = NULL, control = NULL) {
    .solve_TSP(x, method, control)
}

## generic
solve_TSP <- function(x, method = NULL, control = NULL)
    UseMethod("solve_TSP")
    

## workhorse
.solve_TSP <- function(x, method = NULL, control = NULL) {
       
    ## methods
    methods <- c(
        "nearest_insertion",    ## standard
        "farthest_insertion",     
        "greedy",
        "concorde"
    )

    if(is.null(method)) methodNr <- 1
    else methodNr <- pmatch(tolower(method), tolower(methods))
    if(is.na(methodNr)) stop (paste("Unknown method:",sQuote(method)))


    ## work horses
    if(methodNr == 1) {
        order <- tsp_insertion(x, nearest = TRUE, control = control)
    }else if(methodNr == 2) {
        order <- tsp_insertion(x, nearest = FALSE, control = control)
    }else if(methodNr == 3) {
        order <- tsp_greedy(x, control = control)
    }else if(methodNr == 4) {
        order <- tsp_concorde(x, control = control)
    }

    if(!is.integer(order)) order <- as.integer(order)
    class(order) <- c("TOUR", class(order))
    attr(order, "method") <- methods[methodNr]
    attr(order, "tour_length") <- tour_length(x, order)
    names(order) <- labels(x)[order]
    
    return(order)
}
