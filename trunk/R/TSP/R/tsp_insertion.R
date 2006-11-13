## Nearest/farthest insertion algorithm 
## (Johnson and Papadimitrou in Lawler et al. 1985)

tsp_insertion <- function(x, type = "nearest", control = NULL){
            
    ## insertion cost
    insertion_cost <- function(order, k) {
        if(length(order) == 1) cost <- x[order[1], k] 
        else {
            cost <- numeric(length(order))    
            for(i in 1:(length(order)-1)) {
                cost[i] <- x[order[i], k] + x[k, order[i+1]]
                - x[order[i], order[i+1]]
            }
            cost[length(order)] <- x[order[length(order)], k] + x[k,order[1]] 
            - x[order[length(order)],order[1]]
            }
        cost
    }
    
    types <- c("nearest", "farthest", "cheapest", "arbitrary")
    type_num <- pmatch(type, types)
    if(is.na(type_num)) stop(paste("Unknown insertion type: ", sQuote(type)))
    
    ## x comes checked form solve_TSP/solve_ATSP
    n <- n_of_cities(x)

    ## we use a matrix for now (covers TSP and ATSP)
    x <- as.matrix(x)

    start <- control$start
    if(is.null(start)) start <- sample(1:n, 1)
    if(start < 0 || start > n) 
    stop(paste("illegal value for", sQuote("start")))

    placed <- logical(n)
    placed[start] <- TRUE
    order <- c(start)

    while(any(placed == FALSE)) {
        
        ## find city to be inserted
        js <- which(placed)
        ks <- which(!placed)

        ## which.max/which.min do no random tie breaking!
        ## nearest
        if(type_num == 1) {
            mx <- which.min(x[js,ks, drop = FALSE])
            k <- ks[(mx-1) %/% length(js) + 1]
        }
        ## nearest insertion can be implemented more efficiently 
        ## (see Rosenkrantz et al 1977)
        
        ## farthest
        else if(type_num == 2) {
            mx <- which.max(x[js,ks, drop = FALSE])
            k <- ks[(mx-1) %/% length(js) + 1]
        }
       
        ## cheapest
        else if(type_num == 3) {
            k <- ks[which.min(sapply(ks, FUN = 
                    function(k) min(insertion_cost(order, k))))]
        }
        
        ## random
        else if(type_num == 4) k <- if(length(ks) > 1) sample(ks, 1) else ks
        
        ## just in case
        else stop("unknown insertion type")

        ## do insertion
        placed[k] <- TRUE
        if(length(order) == 1) order <- c(order, k)
        else {
            pos <- which.min(insertion_cost(order, k))
            if(pos == length(order)) order <- c(order, k)
            else order <- c(order[1:pos], k, 
                order[(pos+1):length(order)]) 
        }
    }

    order
}


