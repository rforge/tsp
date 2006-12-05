## create a TSP form an ATSP by doubling the cities

ATSP_to_TSP <- function(x, M = Inf, cheap_link = -10) {
    m <- as.matrix(x)
    
    ## scale matrix and add cheap links
    m <- m - min(m) - cheap_link
    diag(m) <- 0

    tsp <- rbind(
        cbind(matrix(M, ncol = ncol(m), nrow = nrow(m)), t(m)),
        cbind(m, matrix(M, ncol = nrow(m), nrow = ncol(m)))
    )

    ## create labels (* for virtual cities)
    lab <- c(labels(x), paste(labels(x), "*", sep = ""))
    dimnames(tsp) <- list(lab, lab)
    TSP(tsp)
}

