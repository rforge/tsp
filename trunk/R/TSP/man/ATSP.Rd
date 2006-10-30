\name{ATSP}
\alias{ATSP}
\alias{n_of_cities.ATSP}
\alias{print.ATSP}
\alias{labels.ATSP}
\alias{image.ATSP}
\title{Class ATSP -- Asymmetric traveling salesperson problem}
\description{
  Constructor to create an instance of an 
  asymmetric traveling salesperson problem (TSP) and 
  some auxiliary methods.
}
\usage{
## constructor
ATSP(x)

## methods
\method{n_of_cities}{ATSP}(x)
\method{image}{ATSP}(x, order, col = gray.colors(64), \ldots)
\method{labels}{ATSP}(object, \ldots)
\method{print}{ATSP}(x, \ldots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, object}{ an object (a square matrix)
      to be converted into an \code{ATSP} or, for the methods, 
      an object of class \code{ATSP}.}
  \item{col}{ color scheme for image. }
  \item{order}{ order of cities for the image as
      an integer vector or an object of class \code{TOUR}.}
  \item{\ldots}{ further arguments are passed on.}
      }
\details{
Objects of class \code{ATSP} are internally represented by a matrix 
(use \code{unclass()} to get just the matrix).
}
\value{
\code{ATSP()} returns \code{x} as an object of class \code{ATSP}.

\code{n_of_cities()} returns an the number of cities in \code{x}.

\code{labels()} returns a vector of the names of the cities in \code{x}.
}
%\references{}
%\author{}
\seealso{
\code{\link{TOUR}},
\code{\link{insert_dummy}},
\code{\link{tour_length}},
\code{\link{solve_ATSP}}.
}
\examples{
data <- matrix(runif(10^2), ncol = 10, dimnames = list(1:10, 1:10))

tsp <- ATSP(data)
tsp

## use some methods for tsp
n_of_cities(tsp)
labels(tsp)

## calculate a tour
order <- solve_ATSP(tsp)

tour_length(tsp)
tour_length(tsp, order)

image(tsp, order)
}
\keyword{classes}% at least one, from doc/KEYWORDS
