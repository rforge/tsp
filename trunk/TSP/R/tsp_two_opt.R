## heuristic to improve a tour using exchanges of 2 edges.

tsp_two_opt <- function(x, control = NULL){

    ## improve a given tour or create a tour using farthest insertion
    tour <- if(!is.null(control$tour)) control$tour 
            else solve_TSP(x, method = "farthest")
    

    tour <- .Call("two_opt", as.dist(x), as.integer(tour))
}

