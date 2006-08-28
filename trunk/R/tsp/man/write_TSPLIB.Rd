\name{write_TSPLIB}
\alias{write_TSPLIB}
\alias{read_TSPLIB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read and write TSPLIB files}
\description{
Reads and writes TSPLIB format files. TSPLIB files can be used by most
TSP solvers. Sample instances for the TSP in TSPLIB format are available 
on the TSPLIB homepage (see references).
}
\usage{
write_TSPLIB(x, file, precision = 6)
read_TSPLIB(file, precision = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{TSP}.}
  \item{file}{ file name. }
  \item{precision}{ controls the number of decimal places used to represent
      distances (see details). If the \code{storage.mode} of \code{x} is
      already \code{integer} this argument is ignored and \code{x} is used as
      is.}
}
\details{
In the TSPLIB format distances are represented by integer values. Therefore, if
\code{x} contains data in \code{storage.mode} \code{double} (which is normal in
R) the values given in \code{x} are multiplied by \eqn{10^{precision}}.  Note
that therefore all results produced by programs using the TSPLIB file as
input need to be divided by \eqn{10^{precision}} (i.e., the decimal
point has to be shifted \code{precision} placed to the left).

Note also that the TSPLIB format cannot handle \code{Inf} which is therefore
replaced by 2 times the maximum value in \code{x} (ignoring the infinity
entries).
}
\value{
\code{read_TSPLIB} returns an object of class \code{TSP}.
}
\references{TSPLIB home page \url{http://www.iwr.uni-heidelberg.de/groups/comopt/software/TSPLIB95/}}
%\author{ ~~who you are~~ }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
%\examples{
%}
\keyword{file}% at least one, from doc/KEYWORDS
