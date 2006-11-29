library("TSP")

n <- 100
tries <- 20
d <- "euclidean"
#d <- "manhattan"

methods <- c("nearest", "farthest", "cheapest", "nn", "repetitive", "concorde")
res <- matrix(0, ncol = length(methods), nrow = 0)

run <- function() {
    data <- matrix(runif(2*n), ncol = 2)
    tsp <- TSP(dist(data, method = d))

    res <<- rbind(res, sapply(methods, FUN = function(m) {
                tour <- solve_TSP(tsp, method = m)
                attr(tour, "tour_length")
            }))
}

replicate(tries, run())
res <- res / res[,"concorde"]
boxplot(as.data.frame(res))
 
