\name{solve_TSP}
\alias{solve_TSP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{TSP solver interface}
\description{
  Common interface to all symmetric TSP solvers in this package.
}
\usage{
solve_TSP(x, method, options, \ldots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ the TSP given as an object of class \code{TSP}.}
  \item{method}{ method to solve the TSP (default: farthest insertion 
      algorithm; see details). }
  \item{options}{ a list of arguments passed on to the TSP solver
      selected by \code{method}.}
  \item{\ldots}{ further arguments (currently unused).}
}
\details{
Currently the following methods are available:
\describe{
    \item{\code{nearest_insertion, farthest_insertion}}{
        Nearest and farthest insertion algorithms for a symmetric TSP (Johnson
        and Papadimitriou 1985).  
        
        The two algorithms are variants of the
        minimum spanning tree algorithm for obtaining solutions to the
        traveling salesman problem.  The distances between cities are stored in
        a distance matrix \eqn{D} with elements \eqn{d(i,j)}.

        The nearest insertion algorithm starts with a partial tour consisting
        of a single, arbitrarily chosen city. As long as not all cities are on
        the tour, find the city \eqn{k} which is \emph{nearest} to a city on
        the tour. Next insert city \eqn{k} into the tour between two
        consecutive cities \eqn{i} and \eqn{j}, such that \eqn{d(i,k) + d(k,j)
        - d(i,j)} is minimized.  Repeat till all cities are on the tour.

        The farthest insertion algorithm used the \emph{farthest} instead of
        the \emph{nearest} city. The idea behind this choice is to link cities
        far away into the tour fist to establish an outline of the whole tour
        early.
        
        Additional options: \code{start} first city (default: random city)
        }
    
    \item{\code{greedy}}{ Greedy algorithm for a symmetric TSP. The algorithm
        is often also called nearest neighbor algorithm (Johnson and
        Papadimitriou 1985).

        The algorithm starts with a tour containing a random city. Then the
        algorithm always adds to last city in the tour the nearest not yet
        visited city. The algorithm stops when all cities are on the tour.
        
        Additional options: \code{start} first city (default: random city)
        }
    
    \item{\code{concorde}}{Concorde algorithm (Applegate et al. 2001).
        
          The executable program Concorde is not included in this package and
          has to be obtained separately from the Concorde web site
          (\url{http://www.tsp.gatech.edu/concorde/downloads/downloads.htm}).
          Unpack the program, place it in a convenient directory and make sure
          it is executable. 

          The following additional options can be used:

          \code{exe:} the whole path to the executable (including the
executable's name) can be given in this option. A more convenient way is to
permanently set the environment variable \code{R\_CONCORDE}.

          \code{clo:} command line options for Concorde can be given a string
in this option, e.g., \code{options = list(clo = "-B -v"} . To get a list of
all available command line options try \code{tsp\_concorde\_help()}.  Several
options (-x, -o, -N) are not available via this package.

          \code{precision:} controls the number of decimal places used for the
internal representation of distances in Concorde. Since Concorde uses
\code{integer} for representing distances, the values given in \code{x} are
multiplied by \eqn{10^{precision}} before being passed on to Concorde. Note
that therefore all the output produced by Concorde (especially lower and upper
bounds) need to be divided by \eqn{10^{precision}} (i.e., the decimal point has
to be shifted \code{precision} placed to the left).  Note also, that Concorde
cannot handle \code{Inf} which is therefore replaced by 2 times the maximum
value in \code{x} (ignoring the infinity entries). See \code{write_TSPLIB}
for more information.
        }
}
}
\value{
  A integer vector containing the order of the cities in \code{x}.
}
\seealso{
\code{\link{TSP}},
\code{\link{write_TSPLIB}}.
}
\references{
Concorde home page \url{http://www.tsp.gatech.edu/concorde/}

David Appletgate, Robert Bixby, Vasek Chvatal, William Cook (2001):
TSP cuts which do not conform to the template paradigm,
Computational Combinatorial Optimization, M. Junger and D. Naddef (editors),
Springer.

D. S. Johnson, C. H. Papadimitriou (1985): Performance guarantees for
heuristics (chapter 5). In: E. L. Lawler, J. K. Lenstra, A.H.G. Rinnooy Kan, D.
B. Shmoys (eds.) The traveling salesman problem - A guided tour of
combinatorial optimization, Wiley \& Sons.
}
%\author{Michael Hahsler}
\examples{
data("iris")

# create TSP
tsp <- TSP(dist(iris[1:4]))

# reorder
order1 <- solve_TSP(tsp)
order1

order2 <- solve_TSP(tsp, method = "greedy", options = list(start = 1)) 
order2

order3 <- solve_TSP(tsp, method = "concorde", options = list(clo = "-v"))
order3
}
\keyword{optimize}% at least one, from doc/KEYWORDS
