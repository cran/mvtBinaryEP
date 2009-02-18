\name{ranMvnXch}
\alias{ranMvnXch}
\title{ Multivariate Normal Data }
\description{
  Generates multivariate normal data based on an EXCHANGEABLE correlation matrix.
}
\usage{
ranMvnXch(rho, n, nRep = 1, seed = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rho}{ correlation }
  \item{n}{ cluster size }
  \item{nRep}{ number of clusters (replications)s }
  \item{seed}{ initializes the random number generator }
}
\details{
  This is more efficient than \code{ranMVN} and \code{ranMVN2} for exchangeable
  correlation.  The returned matrix of responses, call it Y,  has mean 0.  We require that
  \code{rho} is greater than zero.
}
\value{
  Returns a matrix of response variates of dimension \code{nRep} by \code{n}
  }

\seealso{ See Also \code{\link{ranMVN}}, \code{\link{ranMVN2}}, \code{\link{ep}} }

\keyword{ datagen }
\keyword{ distribution }
\keyword{ multivariate }
