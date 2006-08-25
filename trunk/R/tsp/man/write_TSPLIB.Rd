\name{write_TSPLIB}
\alias{write_TSPLIB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a TSPLIB file}
\description{
Writes a TSP in TSPLIB format to a file. TSPLIB files can be used by most
TSP solvers.
}
\usage{
write_TSPLIB(x, file, precision = 6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{TSP}.}
  \item{file}{ file name. }
  \item{precision}{ controls the number of decimal places used to represent distances (see details).}
}
\details{
In the TSPLIB format distances are represented as integers. Therefore, the
values given in \code{x} are multiplied by \eqn{10^{precision}}.  Note that
therefore all the output produced by programms using the TSPLIB file as input
needs to be need to be divided by \eqn{10^{precision}} (i.e., the decimal point
has to be shifted \code{precision} placed to the left).

Note also that the TSPLIB format cannot handle \code{Inf} which is therefore
replaced by 2 times the maximum value in \code{x} (ignoring the infinity
entries).
}
%\value{
%}
\references{TSPLIB home page \url{http://www.iwr.uni-heidelberg.de/groups/comopt/software/TSPLIB95/}}
%\author{ ~~who you are~~ }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
%\examples{
%}
\keyword{IO}% at least one, from doc/KEYWORDS
