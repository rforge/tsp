\name{Chained Lin-Kernighan}
\alias{help_linken}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Concorde's Chained Lin-Kernighan Implementation}
\description{
  Interface to Concord's implementation of the Chained Lin-Kernighan heuristic.
  This solver is freely available for academic research and has to be obtained
  separately from the Concorde web site (see details).
}
\usage{
## obtain a list of command line options for Concorde
help_linkern(exe = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{exe}{ an optional character string containing the whole path to the
      executable (including the executable's name). If this option is not used,
      the linkern executable is expected to be installed
      somewhere in the search path.}
}
\details{
The executable program linkern is not included in this package and has to be
obtained separately from the Concorde web site (see references).  Unpack the
program, place it in a directory which is in the search path.

To get a list of all available command line options which can be used
via the \code{clo} option for \code{solve_TSP} use
\code{help\_linkern()}.  Several options (\option{-x},
\option{-o}, \option{-Q}) are not available via \code{solve_TSP} since
they are used by the interface.  }
%\value{
%}
\seealso{
\code{\link{solve_TSP}},
\code{\link{help_concorde}}.
}
\references{
Concorde home page,
\url{http://www.tsp.gatech.edu/concorde/}

Concorde download page, 
\url{http://www.tsp.gatech.edu/concorde/downloads/downloads.htm}
}
%\author{Michael Hahsler}
%\examples{
%}
}
\keyword{documentation}% at least one, from doc/KEYWORDS
