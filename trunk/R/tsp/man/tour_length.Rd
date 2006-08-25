\name{tour_length}
\alias{tour_length}
\title{Calculate the length of a tour}
\description{
Calculate the length of a tour given a TSP and an order.
  }
\usage{
tour_length(x, order)
}
\arguments{
  \item{x}{an object of class \code{TSP}.}
  \item{order}{ optional order of the visited cities. If no order is given,
      the cities are visited in the original order in \code{x}. }
}
%\details{
%}
\seealso{\code{\link{TSP}}}
%\references{
%}
\examples{
data("iris")
tsp <- TSP(dist(iris[1:4]))

order1 <- tsp_insertion(tsp, nearest = FALSE)
order2 <- tsp_insertion(tsp, nearest = TRUE)
order3 <- tsp_concorde(tsp)

tour_length(tsp)
tour_length(tsp, order1)
tour_length(tsp, order2)
tour_length(tsp, order3)
}
\keyword{optimize}
