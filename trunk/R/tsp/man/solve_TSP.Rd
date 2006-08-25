\name{solve_TSP}
\alias{solve_TSP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{TSP solver interface}
\description{
  Common interface to all symmetric TSP solvers in this package.
}
\usage{
solve_TSP(x, method, \ldots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ the TSP given as an object of class \code{TSP}.}
  \item{method}{ method to solve the TSP (default: farthest insertion 
      algorithm; see details). }
  \item{\ldots}{ further arguments passed on to the TSP solver
      selected by \code{method}.}
}
\details{
Currently the following methods are available:
\describe{
    \item{\code{farthest_insertion}}{Farthest insertion algorithm}
    \item{\code{nearest_insertion}}{Nearest insertion algorithm}
    \item{\code{greedy}}{Nearest neighbor algorithm}
    \item{\code{concorde}}{Concorde}
}
More information about the individual methods is available on the man
pages given in the ``See Also'' section.
}
\value{
  A integer vector containing the order of the cities in \code{x}.
}
\seealso{
\code{\link{tsp_insertion}},
\code{\link{tsp_greedy}},
\code{\link{tsp_concorde}},
\code{\link{TSP}}.
}
%\references{}
%\author{Michael Hahsler}
\examples{
data("iris")

# create TSP
tsp <- TSP(dist(iris[1:4]))

# reorder
order1 <- solve_TSP(tsp)
order1

order2 <- solve_TSP(tsp, method = "greedy", start = 1) 
order2
}
\keyword{optimize}% at least one, from doc/KEYWORDS
