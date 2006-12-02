\name{solve_TSP}
\alias{solve_TSP}
\alias{solve_TSP.TSP}
\alias{solve_TSP.ATSP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{TSP solver interface}
\description{
  Common interface to all TSP solvers in this package.
}
\usage{
solve_TSP(x, method, control)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ the TSP given as an object of class \code{TSP} or \code{ATSP}.}
  \item{method}{ method to solve the TSP (default: nearest insertion 
      algorithm; see details). }
  \item{control}{ a list of arguments passed on to the TSP solver
      selected by \code{method}.}
}
\details{
Currently the following methods are available: 
\describe{
\item{\code{"nearest_insertion"}, 
    \code{"farthest_insertion"},
    \code{"cheapest_insertion"}, 
    \code{"arbitrary_insertion"}
    }{ 
    Nearest,
    farthest, cheapest and arbitrary insertion algorithms for a symmetric and
    asymmetric TSP (Rosenkrantz et al. 1977).  
        
The distances between cities are stored in a distance matrix \eqn{D} with
elements \eqn{d(i,j)}.  All insertion algorithms start with a tour consisting
of an arbitrary city and choose in each step a city \eqn{k} not yet on the
tour. This city is inserted into the existing tour between two consecutive
cities \eqn{i} and \eqn{j}, such that \deqn{d(i,k) + d(k,j) - d(i,j)} is
minimized. The algorithms stops when all cities are on the tour.

The nearest insertion algorithm chooses city \eqn{k} in each step as the city
which is \emph{nearest} to a city on the tour.  

For farthest insertion, the city \eqn{k} is chosen in each step as the city
which is \emph{farthest} to any city on the tour.  

Cheapest insertion chooses the city \eqn{k} such that the cost of inserting the
new city (i.e., the increase in the tour's length) is minimal.  

Arbitrary insertion chooses the city \eqn{k} randomly from all cities not
yet on the tour.
 
Nearest and cheapest insertion tries to build the tour using cities which fit
well into the partial tour constructed so far.  The idea behind behind farthest
insertion is to link cities far away into the tour fist to establish an
outline of the whole tour early.
        
        Additional control options: 
        \describe{\item{\code{start}}{index of the first city 
        (default: random city).}
        }
        }
    
    \item{\code{"nn", "repetitive_nn"}}{ 
        Nearest neighbor and repetitive nearest neighbor algorithms for
        symmetric and asymmetric TSPs (Rosenkrantz et al. 1977).

        The algorithm starts with a tour containing a random city. Then the
        algorithm always adds to the last city on the tour the nearest not yet
        visited city. The algorithm stops when all cities are on the tour. 
 
        Repetitive nearest neighbor constructs a nearest neighbor tour for each
        city as the starting point and returns the shortest tour found.

        Additional control options: 
        \describe{\item{\code{start}}{index of the first city 
        (default: random city).}}
        
        }
    
    \item{\code{"concorde"}}{Concorde algorithm (Applegate et al. 2001).
        
        Concorde is an advanced TSP solver for \emph{only symmetric} TSPs. The
          program is not included in this package and has to be obtained and
          installed separately (see \code{\link{help_concorde_}}).

          Additional control options: 
      \describe{
  \item{\code{exe}}{ a character string containing the path to the executable 
      (see \code{help_concorde}).}

  \item{\code{clo}}{ a character string containing command line options for
Concorde, e.g., \code{control = list(clo = "-B -v")}. See
\code{help_concorde} on how to obtain a complete list of available
command line options.} 

         \item{\code{precision}}{ an integer which controls the number of
decimal places used for the internal representation of distances in Concorde.
The values given in \code{x} are multiplied by \eqn{10^{precision}} before
being passed on to Concorde. Note that therefore the results produced by
Concorde (especially lower and upper bounds) need to be divided by
\eqn{10^{precision}} (i.e., the decimal point has to be shifted
\code{precision} placed to the left). Note also, that Concorde cannot handle
\code{Inf} which is therefore replaced by 2 times the maximum value in \code{x}
(ignoring the infinity entries). The interface to Concorde uses 
\code{write_TSPLIB} (see there for more information).}
        
}
}
}
}
\value{
An object of class \code{TOUR}.
}
\seealso{
\code{\link{TOUR}},
\code{\link{TSP}},
\code{\link{ATSP}},
\code{\link{write_TSPLIB}},
\code{\link{help_concorde}}.
}
\references{
Concorde home page, \url{http://www.tsp.gatech.edu/concorde/}

David Appletgate, Robert Bixby, Vasek Chvatal, William Cook (2001):
TSP cuts which do not conform to the template paradigm,
Computational Combinatorial Optimization, M. Junger and D. Naddef (editors),
Springer.

D. J. Rosenkrantz, R. E. Stearns, and Philip M. Lewis II (1977): 
An analysis of several heuristics for the traveling salesman problem. 
\emph{SIAM Journal on Computing, 6(3):563--581.}
  


}
%\author{Michael Hahsler}
\examples{
data("USCA50")

## create TSP
tsp <- USCA50

## calculate tours
tours <- list()
tours$nearest   <- solve_TSP(tsp)
tours$farthest  <- solve_TSP(tsp, method = "farthest_insertion")
tours$cheapest  <- solve_TSP(tsp, method = "cheapest_insertion")
tours$arbitrary    <- solve_TSP(tsp, method = "arbitrary_insertion")
tours$nn        <- solve_TSP(tsp, method = "nn")
tours$repetitive_nn    <- solve_TSP(tsp, method = "repetitive_nn")

## use the external solver which has to be installed separately
\dontrun{
tours$concorde  <- solve_TSP(tsp, method = "concorde")
}

tours

## compare tour lengths
barplot(sapply(tours, FUN = attr, "tour_length"), ylab = "tour length")

opt <- 14497 # optimal tour length
abline(h = opt, col = "red")
mtext("optimum", at = opt, side = 4, col = "red")


}
\keyword{optimize}% at least one, from doc/KEYWORDS
