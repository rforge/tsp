\name{ATSP_to_TSP}
\alias{ATSP_to_TSP}
\title{Reformulate a ATSP as a symmetric TSP}
\description{
A ATSP can be formulated as a symmetric TSP by doubling the number of
cities (Jonker and Volgenant 1983). Then the TSP can be solved 
which also solves the original ATSP.
}
\usage{
ATSP_to_TSP(x, M = Inf, cheap_link = -10)
}
\arguments{
  \item{x}{an ATSP.}
  \item{M}{value for impossible edges.}
  \item{cheap_link}{value for the link between a city and the corresponding 
      dummy city.}
}
\details{
To reformulate the ATSP as a TSP, for each city a dummy city (e.g, for
'New York' a dummy city 'New York*') is added. Between each city and its
corresponding dummy city a distance with value \code{cheap_link} is used.
This makes sure that cities and dummy cities will always occur together
in the solution. The original distances are used 
between the cities and the dummy cities, 
where the cities are responsible for the distances going to the city and 
the dummy cities are responsible for the distances coming from the
city. The distances between all cities or between all dummy cities are
set to \code{M}, a very large value which makes the infeasible.

%The result is a distance matrix for the TSP of the following form:
%
%\eqn{\begin{pmatrix}\infty & D'^T \\ D' & \infty \end{pmatrix},}{}
%
%where \eqn{D'} is the original distance matrix with
%the diagonal replaced by the value for the  \code{cheap_link}.
}
\value{
a TSP object.
}
\references{
Jonker, R. and Volgenant, T. (1983): Transforming asymmetric into symmetric
traveling salesman problems, \emph{Operations Research Letters, 2, 161--163.}
}
\seealso{
\code{\link{ATSP}},
\code{\link{TSP}}.
}
\examples{
data("USCA50")

austin <- which(labels(USCA50) == "Austin, TX")

atsp <- as.ATSP(USCA50)
atsp[, austin] <- 0

tsp <- ATSP_to_TSP(atsp)
labels(tsp)

tour <- solve_TSP(tsp)
labels(tour)
}
\keyword{optimize}
