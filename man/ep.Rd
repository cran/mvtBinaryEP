\name{ep}
\alias{ep}
\title{ Correlated Binary Data }
\description{
  Generates correlated binary data based on the method of Emrich and Piedmonte (1991)
}
\usage{
ep(mu, R, rho, n, isd = NULL, nRep = 1, seed = NULL, crit = 1e-06,
maxiter = 20)
}

\arguments{
  \item{mu}{ Vector of means.  If \code{rho} is specified then \code{mu} must be of length 1. }
  \item{R}{ Correlation matrix. If \code{rho} is specified then \code{R} is ignored. Only the
            upper part of R is used. }
  \item{rho}{ If common mean and exchangeable correlation is desired, then this correlation parameter must
              be specified. }
  \item{n}{ Cluster size.  If \code{rho} is specified, then this must be specified as well. }
  \item{isd}{ Internal Simulation Descriptor. This is useful for generating more responses 
              based on the parameters used in the prior call to \code{ep}.  This increases
              efficiency since the intermediate quantities need not be recomputed.  \code{isd} is 
              a \bold{list} containing some of the input parameters as well as some intermediate
              quantities.  If this is provided then \code{R} and \code{rho} are ignored. }
  \item{nRep}{ Number of clusters (replications). }
  \item{seed}{ Sets the seed }
  \item{crit}{ Level of precision used in solving for the tetra-choric correlations. }
  \item{maxiter}{ Maximum number of iterations used in solving for the tetra-choric correlations. }
}

\details{ The method relies on simulating multivariate normal vectors and then dichotomizing each 
          coordinate.  The cutpoints are determined by \code{mu}.  The correlation matrix \code{S} 
          (which are the tetra-choric correlations) of the multivariate normal vectors is computed
          in such a way that the resulting binary vectors have correlation matrix \code{R}. One possible
          complication is that this process is not always possible.  Further, when all tetra-choric 
          correlations are computed, the resulting matrix, \code{S}, may not be positive definite.  These
          are two possible failure points in the algorithm; both are detected and reported back by the code.  
}

\value{
  Returns a list with the following two components:
  \item{y }{ Multivariate response of dimension \code{nRep} by \code{length(mu)}}
  \item{isd }{ Internal Simulation Descriptor }
}

\note{ The returned object \bold{isd} is also a list with the following fields:
    \itemize{
    \item{\code{mu} }{ input parameter }
    \item{\code{rho} }{ input parameter }
    \item{\code{n} }{ input parameter }
    \item{\code{R} }{ input parameter }
    \item{\code{rootS} }{ The Cholesky root of the tetrachoric correlation matrix S (if positive definite) }
    \item{\code{S} }{ The tetrachoric correlation matrix S (if NOT positive definite) }
    \item{\code{pd} }{ Flag, TRUE if S is positive definite, FALSE otherwise }
    \item{\code{sp} }{ Flag, TRUE if successful in solving for tetrachoric correlations }
    \item{\code{i} }{ row where solving for the tetrachoric correlation failed, if it did fail }
    \item{\code{j} }{ column where solving for the tetrachoric correlation failed, if it did fail }
    }
}

\seealso{ See Also \code{\link{ranMVN}}, \code{\link{ranMVN2}}, \code{\link{ranMvnXch}} }

\examples{

# Create mean vector
mu=c(0.5, 0.3, 0.20, 0.1)

# Create correlation matrix
R = c(
    1    , 0.2 , 0.15, -0.05,
    0.2  , 1   , 0.25, 0.2  , 
    0.15 , 0.25, 1   , 0.25 ,
    -0.05, 0.2 , 0.25, 1
)
R = matrix(R, ncol=4)


ep0 = ep(mu=mu, R=R, nRep=1000, seed=NULL)
apply(ep0$y, 2, mean); cor(ep0$y)

#Generates more responses based on the parameters provided above.
ep1 = ep(isd = ep0$isd, nRep=1000, seed=NULL)
apply(ep1$y, 2, mean); cor(ep1$y)


# 5-variate based on common mean and exchangeable correlation.
ep2 = ep(mu=0.3, rho=0.2, n=5, nRep=10000)
apply(ep2$y, 2, mean); cor(ep2$y)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ datagen }
\keyword{ distribution }
\keyword{ multivariate }


