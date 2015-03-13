#######################################################################
# TSP - Traveling Salesperson Problem 
# Copyrigth (C) 2011 Michael Hahsler and Kurt Hornik
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.



## TSP
solve_TSP.TSP <- function(x, method = NULL, control = NULL, ...) {
  .solve_TSP(x, method, control, ...)
}

## ATSP
solve_TSP.ATSP <- function(x, method = NULL, control = NULL, ...) {
  .solve_TSP(x, method, control, ...)
}

## ETSP
solve_TSP.ETSP <- function(x, method = NULL, control = NULL, ...) {
  .solve_TSP(as.TSP(x), method, control, ...)
}

## generic
solve_TSP <- function(x, method = NULL, control = NULL, ...)
  UseMethod("solve_TSP")


## workhorse
.solve_TSP <- function(x, method = NULL, control = NULL, ...) {
  
  ## check for NAs
  if(any(is.na(x))) stop("NAs not allowed!")
  
  ## add ... to control
  control <- c(control, list(...))
  
  ## methods
  methods <- c(
    "nearest_insertion",
    "farthest_insertion",     
    "cheapest_insertion",     
    "arbitrary_insertion",     
    "nn",
    "repetitive_nn",
    "2-opt", ### deprecated
    "two_opt",
    "concorde",
    "linkern"
  )
  
  ## default is arbitrary_insertion + two_opt
  if(is.null(method)) {
    method <- "arbitrary_insertion"
    if(is.null(control$two_opt)) control <- c(control, list(two_opt = TRUE))
  } else method <- match.arg(tolower(method), methods)
  
  
  ## punish (-)Inf with max (min) +(-) 2*range 
  x_orig <- x
  if(any(is.infinite(x))) { 
    range_x <- range(x, na.rm = TRUE, finite = TRUE)
    pInf <- range_x[2] + 2*(range_x[2] - range_x[1])
    nInf <- range_x[1] - 2*(range_x[2] - range_x[1])
    x[x == Inf] <- pInf
    x[x == -Inf] <- nInf
  }
  
  ## work horses
  .solve_TSP_worker <- function() {
    order <- switch(method,
      concorde = tsp_concorde(x, control = control),
      linkern = tsp_linkern(x, control = control),
      nearest_insertion = tsp_insertion(x, type = "nearest", control = control),
      farthest_insertion = tsp_insertion(x, type = "farthest", control = control),
      cheapest_insertion = tsp_insertion(x, type = "cheapest", control = control),
      #      arbitrary_insertion = tsp_insertion(x, type = "arbitrary", control = control),
      arbitrary_insertion = tsp_insertion_arbitrary(x, control = control),
      nn = tsp_nn(x, control = control),
      repetitive_nn = tsp_repetitive_nn(x, control = control),
      two_opt = tsp_two_opt(x, control = control),
      '2-opt' = tsp_two_opt(x, control = control)
    )
    
    ### do refinement two_opt
    if(!is.null(control$two_opt) && control$two_opt) { 
      order <- tsp_two_opt(x, control = list(tour = order))
      method <- paste(method , "+two_opt", sep = "")
    }
    
    TOUR(order, method=method, tsp=x_orig)
  }
  
  ## do rep?
  if(!is.null(control$rep)) n <- control$rep
  else n <- 1    
  if(method == "concorde" || method == "linkern") n <- 1
  if(method == "repetitive_nn") n <- 1

  if(n==1) return(.solve_TSP_worker())
  
  l <- replicate(n, .solve_TSP_worker(), simplify = FALSE)
  l <- l[[which.min(sapply(l, attr, "tour_length"))]]
  attr(l, "method") <- paste(attr(l, "method"), "_rep_", n, sep="")
  return(l)
}
