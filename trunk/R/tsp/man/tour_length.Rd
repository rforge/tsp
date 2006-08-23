\name{tour_length}
\alias{tour_length}
\title{Calculate the length of a tour}
\description{
 Calculate the length of a tour given a symmetric distance (dissimilarity)
 matrix and an order.
  }
\usage{
tour_length(dist, order)
}
\arguments{
  \item{dist}{an object of class \code{dist}.}
  \item{order}{ optional order of the visited cities. If no order is given,
      the cities are visited in the order given in \code{dist}. }
}
%\details{
%}
%\seealso{}
%\references{
%}
\examples{
data("iris")
d <- dist(iris[1:4])

order1 <- tsp_insertion(d, nearest = TRUE)
order2 <- tsp_insertion(d, nearest = FALSE)
order3 <- tsp_concorde(d)

tour_length(d)
tour_length(d, order1)
tour_length(d, order2)
tour_length(d, order3)
}
\keyword{optimize}
