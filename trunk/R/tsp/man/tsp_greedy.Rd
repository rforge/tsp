\name{tsp_greedy}
\alias{tsp_greedy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Greedy/nearest neighbor algorithm}
\description{
  Greedy algorithm for a symmetric TSP. The algorithm is often also called
  nearest neighbor algorithm (Johnson and Papadimitriou 1985). 
}
\usage{
tsp_greedy(x, start = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ the TSP given as a \code{dist} object.}
  \item{start}{\code{integer}; the index of the first city to place
      (default: random starting position).}
}
\details{
The algorithm starts with a tour containing a random city. Then the algorithm
always adds to last city in the tour the nearest not yet visited city. The
algorithm stops when all cities are on the tour.
}
\value{
  A integer vector containing the order of the objects in \code{x}.
}
\references{
D. S. Johnson, C. H. Papadimitriou (1985): Performance guarantees for
heuristics (chapter 5). In: E. L. Lawler, J. K. Lenstra, A.H.G. Rinnooy Kan, D.
B. Shmoys (eds.) The traveling salesman problem - A guided tour of
combinatorial optimization, Wiley \& Sons.
}
%\author{Michael Hahsler}
%\seealso{\code{\link{tsp_insertion}}}
\examples{
data("iris")
d <- dist(iris[1:4])

order <- tsp_greedy(d)
image(as.matrix(d)[order, order], col = gray.colors(64))
}
\keyword{optimize}% at least one, from doc/KEYWORDS
