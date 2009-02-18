\name{ranMVN}
\alias{ranMVN}

\title{ Multivariate Normal Data }
\description{
  Generates multivariate normal data based on a specified covariance matrix.
}
\usage{
ranMVN(nRep = 1, Sigma, seed = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nRep}{ Numer of clusters (replications) }
  \item{Sigma}{ Specified covariance matrix }
  \item{seed}{ Initialize random number generator }
}
\details{
  The returned matrix of responses, call it Y,  has mean 0.
}
\value{
  Returns a matrix of response variates of dimension \code{nRep} by \code{nrow(Sigma)}
}

\seealso{ See Also \code{\link{ranMVN2}}, \code{\link{ranMvnXch}}, \code{\link{ep}} }

\examples{
mu = c(25.3, 28.4, 35.7, 50.2)
c1 = c(17, 11, 8, 10)
c2 = c(11, 17, 9, 8)
c3 = c(8, 9, 17, 9)
c4 = c(10, 8, 9, 17)
S = rbind(c1, c2, c3, c4)


y = mu + ranMVN(nRep = 25, S, seed = NULL)
}
\keyword{ datagen }
\keyword{ distribution }
\keyword{ multivariate }
