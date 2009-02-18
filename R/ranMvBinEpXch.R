`ranMvBinEpXch` <-
function(u, r, p, nReps, crit=1e-6, maxiter=20, seed)
{
  tcor <- tetra1(c(u,u), r, crit=crit, maxiter=maxiter)
  if (tcor[[2]])
  {
    y <- NULL
    out <- list(y = y, tcc = tcor[[1]], fail = tcor[[2]])
    return(out)
  }
  pd <- as.logical((tcor[[1]] > (-1/(p-1))))
  if (pd)
  {
    if (missing(seed)) {z <- ranMvnXch(rho=tcor[[1]], n=p, nRep=nReps)}
    else {z <- ranMvnXch(rho=tcor[[1]], n=p, nRep=nReps, seed=seed)}
    y <- ifelse(z <= qnorm(u),1,0)
  }
  else 
  {
    y <- NULL
    tcor[[2]] <- TRUE
    warning("tetra-choric correlation matrix is not positive definite")
  }
  out <- list(y = y, tcc=tcor[[1]], fail=tcor[[2]])
  return(out) 
}

