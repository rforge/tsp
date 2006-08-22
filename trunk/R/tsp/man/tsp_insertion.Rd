\name{tsp_insertion}
\alias{tsp_insertion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Nearest/farthest insertion algorithm}
\description{
  Nearest and farthest insertion algorithms 
  for a symmetric TSP (Johnson and Papadimitriou 1985) 
  given by a \code{dist} object.
}
\usage{
tsp_insertion(x, nearest = TRUE, start = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ the TSP given as a \code{dist} object.}
  \item{nearest}{\code{logical}; should nearest insertion (default)
      or farthest insertion be used.}
  \item{start}{\code{integer}; the index of the first city to place
      (default: random starting position).}
}
\details{
The two algorithms are variants of the minimum spanning tree algorithm
for obtaining solutions to the traveling salesman problem.
The distances
between cities are stored in a dissimilarity matrix \eqn{D} with elements
\eqn{d(i,j)}.

      The nearest insertion algorithm starts with a partial tour consisting of
a single, arbitrarily chosen observation. As long as not all observations are
in the tour, find the city \eqn{k} which is \emph{nearest} to a city on the
tour. Next insert city \eqn{k} into the tour between two consecutive cities
\eqn{i} and \eqn{j}, such that \eqn{d(i,k) + d(k,j) - d(i,j)} is minimized.
Repeat till all cities are on the tour.

      The farthest insertion algorithm used the \emph{farthest} instead of the
\emph{nearest} city. The idea behind this choice is to link cities far away
into the tour fist to establish an outline of the whole tour early.
}
\value{
  A integer vector containing the order of the objects in \code{dist}.
}
\references{
D. S. Johnson, C. H. Papadimitriou (1985): Performance guarantees for
heuristics (chapter 5). In: E. L. Lawler, J. K. Lenstra, A.H.G. Rinnooy Kan, D.
B. Shmoys (eds.) The traveling salesman problem - A guided tour of
combinatorial optimization, Wiley \& Sons.
}
%\author{Michael Hahsler}
\seealso{\code{\link{tsp_concorde}}}
\examples{
data("iris")
d <- dist(iris[1:4])

order <- tsp_insertion(d)
image(as.matrix(d)[order, order])
}
\keyword{optimize}% at least one, from doc/KEYWORDS
