`ranMvBinEp2` <-
function(rootS, u, nReps, seed)
{
  if (missing(seed)){z <- ranMVN2(nRep=nReps, rootS)}
  else {z <- ranMVN2(nRep=nReps, rootS,seed=seed)}
  cuts <- matrix(rep(qnorm(u),nReps),nReps,byrow=T)
  y <- ifelse(z <= cuts, 1 ,0)
  return(y)
}

