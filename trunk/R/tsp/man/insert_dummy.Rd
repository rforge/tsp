\name{insert_dummy}
\alias{insert_dummy}
\alias{insert_dummy.matrix}
\alias{insert_dummy.dist}
\title{Insert a dummy city into a dissimilarity matrix}
\description{
  Provides methods for 
  inserting dummy cities into a dissimilarity matrix.
  Dummy cities can be used together with TSP solvers to
  find an optimal linear order or to perform
  rearrangement clustering (Climer and Zhang 2006).
  }
\usage{
insert_dummy(x, n = 1, const = max(x), inf = Inf)
}
\arguments{
  \item{x}{an object of class \code{matrix} or \code{dist}.}
  \item{n}{number of dummy cities.}
  \item{const}{dissimilarity of the dummy cities to all other cities.}
  \item{inf}{dissimilarity between dummy cities.}
}
\details{
The dummy cities are insert before the other cities. 
}
%\seealso{}
\references{
Sharlee Climer, Weixiong Zhang (2006): Rearrangement Clustering: Pitfalls,
Remedies, and Applications, Journal of Machine Learning Research 7, pp.
919-943.
}
\examples{
data("iris")
d <- dist(iris[1:4])

# insert 2 dummy cities (as object 1 and 2)
d <- insert_dummy(d, n = 2)

# get a solution for the TSP
o <- tsp_insertion(d, nearest = TRUE)

# plot the distance matrix
p <- attr(d, "Size")
image(1:p, 1:p ,as.matrix(d) [o,o], col = gray.colors(64))

# draw lines where the dummy cities are located
abline(h=sapply(1:2, FUN = function(i) which(o == i)))
abline(v=sapply(1:2, FUN = function(i) which(o == i)))
}
\keyword{manip}
