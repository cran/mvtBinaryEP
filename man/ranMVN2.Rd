\name{ranMVN2}
\alias{ranMVN2}

\title{ Multivariate Normal Data }
\description{
  Generates multivariate normal data based on the Cholesky root of the covariance matrix.
}
\usage{
ranMVN2(nRep = 1, rootS, seed = NULL)
}

\arguments{
  \item{nRep}{ Numer of clusters (replications) }
  \item{rootS}{ Cholesky root of the desired covariance matrix }
  \item{seed}{ Initialize random number generator }
}
\details{
  The matrix of response, call it Y, has mean 0.
}
\value{
  Returns a matrix of response variates of dimension \code{nRep} by \code{nrow(rootS)}
}

\seealso{ See Also \code{\link{ranMVN}}, \code{\link{ranMvnXch}}, \code{\link{ep}} }

\examples{
mu = c(25.3, 28.4, 35.7, 50.2)
c1 = c(17, 11, 8, 10)
c2 = c(11, 17, 9, 8)
c3 = c(8, 9, 17, 9)
c4 = c(10, 8, 9, 17)
S = rbind(c1, c2, c3, c4)

rootS=chol(S)

y = mu + ranMVN2(nRep = 25, rootS, seed = NULL)

}

\keyword{ datagen }
\keyword{ distribution }
\keyword{ multivariate }
