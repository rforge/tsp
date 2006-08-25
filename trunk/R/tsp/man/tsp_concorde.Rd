\name{tsp_concorde}
\alias{tsp_concorde}
\alias{tsp_concorde_help}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Call Concorde to solve a TSP}
\description{
  Calls Concorde to solve a TSP.
}
\usage{
tsp_concorde(x, options = "", precision = 6, exe = Sys.getenv("R_CONCORDE"))
tsp_concorde_help(exe = Sys.getenv("R_CONCORDE"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ the TSP given as an object of class \code{TSP}.}
  \item{options}{ options for Concorde (see details).}
  \item{precision}{ how many decimal places are used for the distances 
      (see details).}
  \item{exe}{ the path to the Concorde executable (see details).}
}
\details{
  The executable program Concorde is not included in this package and has to
be obtained separately from the Concorde web site
(\url{http://www.tsp.gatech.edu/concorde/downloads/downloads.htm}).  Unpack the
program, place it in a convenient directory and make sure it is executable.
The whole path to the executable (including the executable's name) can be given
as \code{exe}. A more convenient way is to permanently set the
environment variable \code{R\_CONCORDE}.

Options for Concorde can be passed to \code{tsp\_concorde} as a string in
\code{options}, e.g., \code{options = "-B -v"} .  To get a list of what options
are available try \code{tsp\_concorde\_help}. Several options (-x, -o, -N) are
not available via this package.

Precision controls the number of decimal places used for the
internal representation of distances in Concorde. Since Concorde uses
\code{integer} for representing distances, the values given in \code{x}
are multiplied by \eqn{10^{precision}} before being passed on to Concorde. Note
that therefore all the output produced by Concorde (especially lower and upper
bounds) need to be divided by \eqn{10^{precision}} (i.e., the decimal point has
to be shifted \code{precision} placed to the left).

Note also, that Concorde cannot handle \code{Inf} which is therefore replaced
by 2 times the maximum value in \code{x} (ignoring the infinity entries).
}
\value{
  A integer vector containing the order of the cities in \code{x}.
  The returned order can be optimal -- read the output of Concorde.
}
\seealso{
    \code{\link{TSP}}
}
\references{
Concorde home page \url{http://www.tsp.gatech.edu/concorde/}

David Appletgate, Robert Bixby, Vasek Chvatal, William Cook (2001): 
TSP cuts which do not conform to the template paradigm,
Computational Combinatorial Optimization, M. Junger and D. Naddef (editors),
Springer.
}
%\author{Michael Hahsler}
\examples{
data("iris")
tsp <- TSP(dist(iris[1:4]))

# use Concorde to get a very good solution
order <- tsp_concorde(tsp)

# plot the reordered distance matrix
image(tsp, order)
}
\keyword{optimize}% at least one, from doc/KEYWORDS
