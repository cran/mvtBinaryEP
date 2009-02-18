`tetra1` <-
function(mu,r12,crit=1e-6, maxiter=20)
{
  if ( missing(mu) ) {stop('You need to provide the mean vector')}
  if (r12==0)
    {
      L <- c(0,0,0)
      return(L)
#      stop("The pairwise correlation 'r12' need to be > 0")
    }
  # find [a,b] that brackets the root
  if (r12>0){a <- 0; b <- 1}
  else {a <- -1; b <- 0}
  u12 <- mu[1]*mu[2]  +  r12*sqrt( mu[1]*(1-mu[1]) * mu[2]*(1-mu[2]) )
  x1 <- qnorm(mu[1])
  x2 <- qnorm(mu[2])
  tcc <- r12
  #============================================================================#
  # Repeated bisection to locate the root in [a,b].
  #============================================================================#
  niter <- 0
  cdf <- pmvnorm( lower=c(-Inf, -Inf), upper=c(x1,x2), corr=toeplitz(c(1,tcc)) )
  error <- cdf - u12
  fail <- ( abs(error) >= crit )
  while(fail && (niter < maxiter))
    {
      if (error > 0) {b <- tcc}
      else {a <- tcc}
      tcc <- a + 0.5*(b-a)
      cdf <- pmvnorm( lower=c(-Inf, -Inf), upper=c(x1,x2),corr=toeplitz(c(1,tcc)) )
      error <- cdf - u12
      fail <- (abs(error) >= crit)
      niter <- niter + 1;
    }
    L <- c(tcc, fail, niter)
    return(L)
}

