\name{tour_length}
\alias{tour_length}
\alias{tour_length.TSP}
\alias{tour_length.ATSP}
\title{Calculate the length of a tour}
\description{
Calculate the length of a tour given a TSP and an order.
  }
\usage{
tour_length(x, order)
}
\arguments{
  \item{x}{an object of class \code{TSP} or \code{ATSP}.}
  \item{order}{ optional order of the visited cities
      as a integer vector or an object of class \code{TOUR}.  If no order is
      given, the cities are visited in the original order in \code{x}. }
}
%\details{
%}
\seealso{
\code{\link{TSP}},
\code{\link{ATSP}},
\code{\link{TOUR}}.
}
%\references{
%}
\examples{
data("USCA50")

## original order
tour_length(USCA50)

## random tour
tour_length(USCA50, sample(1:n_of_cities(USCA50)))
}
\keyword{optimize}
