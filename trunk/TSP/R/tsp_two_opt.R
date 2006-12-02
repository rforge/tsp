## heuristic to improve a tour using exchanges of 2 edges.

tsp_two_opt <- function(x, control = NULL){

    ## improve a given tour or create a random tour
    tour <- if(!is.null(control$tour)) control$tour else sample(n_of_cities(x)) 

    tour <- .Call("two_opt", as.dist(x), as.integer(tour))
}

