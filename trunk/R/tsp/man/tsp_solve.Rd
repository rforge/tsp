\name{tsp_solve}
\alias{tsp_solve}
\alias{tsp_help}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Call Concorde to solve a TSP}
\description{
  Calls Concorde to solve a TSP given by a \code{dist} object.
}
\usage{
tsp_solve(dist, options = "", exe = Sys.getenv("R_CONCORDE"), wd = "/tmp/")
tsp_help(exe = Sys.getenv("R_CONCORDE"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dist}{ the TSP given as a \code{dist} object.}
  \item{options}{ options for Concorde (see details).}
  \item{exe}{ the path to the Concorde executable (see details).}
  \item{wd}{ working directory for Concorde.}
}
\details{
  The executable program Concorde is not included in this package and has to
be obtained separately from the Concorde web site
(\url{http://www.tsp.gatech.edu/concorde/downloads/downloads.htm}).  Unpack the
program, place it in a convenient directory and make sure it is executable.
The whole path to the executable (including the executable's name) can be given
as \code{exe} to \code{tsp\_solve}. A more convenient way is to set the
environment variable \code{R\_CONCORDE}.

  Options for Concorde can be passed to \code{tsp\_solve} as a string in
  \code{options}, e.g., \code{options = "-B -v"} .  To get a list of what
  options are available try \code{tsp\_help}. Several options (-x, -o, -N) are
  not available via this package.
}
\value{
  A integer vector containing the order of the objects in \code{dist}.
  The returned order can be optimal -- read the output of Concorde.
}
\references{
Concorde home page \url{http://www.tsp.gatech.edu/concorde/}

David Appletgate, Robert Bixby, Vasek Chvatal, William Cook (2001): 
TSP cuts which do not conform to the template paradigm,
Computational Combinatorial Optimization, M. Junger and D. Naddef (editors),
Springer.
}
\author{Michael Hahsler}
\seealso{\code{\link{Sys.putenv}}}
\examples{
data("iris")
d <- dist(iris[1:4])

order <- tsp_solve(d)
image(as.matrix(d)[order, order])
}
\keyword{optimize}% at least one, from doc/KEYWORDS
