library(TSP)
library(testthat)

### INF
m <- rbind(
	   c(0, 1, 0, 1),
	   c(1, 0, 1, Inf),
	   c(0, 1, 0, 1),
	   c(1, Inf, 1, 0)
	   )
d <- as.dist(m)
tsp <- TSP(d)

## test error on NA
tsp_na <- tsp
tsp_na[4] <- NA
expect_error(o <- solve_TSP(tsp_na))

## test Inf
methods <- c("nearest_insertion", "cheapest_insertion", "farthest_insertion", 
    "arbitrary_insertion", "nn", "repetitive_nn", "two_opt")

tours <- lapply(methods, FUN = function(m) solve_TSP(tsp, method = m))
names(tours) <- methods
tours

sapply(tours, function(o) tour_length(tsp, o))

##
o <- solve_TSP(tsp, method="concorde")
as.integer(o)
tour_length(tsp, o)