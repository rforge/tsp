## Insertion algorithms 
## (Rosenkrantz et al. 1977)

tsp_insertion <- function(x, type = "nearest", control = NULL){
            
    types <- c("nearest", "farthest", "cheapest", "arbitrary")
    type_num <- pmatch(type, types)
    if(is.na(type_num)) stop(paste("Unknown insertion type: ", sQuote(type)))
    
    ## x comes checked form solve_TSP/solve_ATSP
    n <- n_of_cities(x)

    ## we use a matrix for now (covers TSP and ATSP)
    asym <- inherits(x, "ATSP")
    x <- as.matrix(x)

    ## prepare criterion for nearest/farthest
    if(type_num == 1) crit <- which.min 
    if(type_num == 2) crit <- which.max 

    ## place first city
    start <- control$start
    if(is.null(start)) start <- sample(1:n, 1)
    if(start < 0 || start > n) 
    stop(paste("illegal value for", sQuote("start")))

    placed <- logical(n)
    placed[start] <- TRUE
    order <- c(start)

    ## place other cities
    while(any(placed == FALSE)) {
        
        ## find city to be inserted
        ks <- which(!placed)
        js <- which(placed)

        ## which.max/which.min do no random tie breaking!
        ## nearest / farthest
        if(type_num < 3) {
            m <- x[ks,js, drop = FALSE]
            
            ## for the asymmetric case we have to take distances
            ## from and to the city into account
            if(asym){
                m <- cbind(m, t(x)[ks,js, drop = FALSE]) 
            }
            
            ds <- sapply(1:length(ks), FUN = 
                function(i)  min(m[i, , drop = FALSE]))

            winner_index <- crit(ds)
            ## in case it was all Inf/-Inf
            if(length(winner_index) == 0) winner_index <- 1
            k <- ks[winner_index] 
        }
       
        ## cheapest
        else if(type_num == 3) {
            winner_index <- which.min(sapply(ks, FUN =
                    function(k) min(.Call("insertion_cost", x, order, k))))
            ## in case it was all Inf/-Inf
            if(length(winner_index) == 0) winner_index <- 1
            k <- ks[winner_index]

            ## we look for the optimal insertion place for k again later
            ## this is not necessary, but it is more convenient
            ## to reuse the code for the other insertion algorithms for now. 
        }
        
        ## random
        else if(type_num == 4) k <- if(length(ks) > 1) sample(ks, 1) else ks
        
        ## just in case
        else stop("unknown insertion type")

        ## do insertion
        placed[k] <- TRUE
        
        if(length(order) == 1) order <- append(order, k)
        else {
            pos <- which.min(.Call("insertion_cost", x, order, k))
            if(length(pos) == 0) pos <- 1 ### in case we only have Inf and
                                          ### so which.min does not do it
            order <- append(order, k, after = pos)
        }
    }

    order
}


