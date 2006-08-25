\name{TSP}
\alias{TSP}
\alias{cities}
\alias{cities.TSP}
\alias{print.TSP}
\alias{labels.TSP}
\alias{image.TSP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Class TSP -- Symmetric traveling salesman problem}
\description{
  Constructor to create an instance of a 
  symmetric traveling salesman problem (TSP) 
  and some auxiliary methods.
}
\usage{
# constructor
TSP(x)

# methods
\method{cities}{TSP}(x)
\method{image}{TSP}(x, order, col = gray.colors(64), \ldots)
\method{labels}{TSP}(object, \ldots)
\method{print}{TSP}(x, \ldots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, object}{ an object (currently \code{dist} or a symmetric
      \code{matrix}) to be converted into a \code{TSP} or an
      object of class \code{TSP}.}
  \item{col}{ color scheme for image. }
  \item{order}{ order of cities for the image, e.g., a tour computed by 
      a TSP solver. }
  \item{\ldots}{ further arguments are passed on.}
      }
%\details{
%}
\value{
\code{TSP()} returns \code{x} as an object of class \code{TSP}.

\code{cities()} returns an the number of cities in \code{x}.

\code{labels()} returns a vector of the names of the cities in \code{x}.
}
%\references{}
%\author{}
%\seealso{}
\examples{
data("iris")
d <- dist(iris[1:4])
m <- as.matrix(d)

# create a TSP
tsp1 <- TSP(d)
tsp2 <- TSP(m)

tsp1
tsp2

# some methods for tsp1
cities(tsp1)

labels(tsp1)

image(tsp1)
}
\keyword{classes}% at least one, from doc/KEYWORDS
