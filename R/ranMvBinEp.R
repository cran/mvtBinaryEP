`ranMvBinEp` <-
function(u, R, nReps, crit=1e-6, maxiter=20, seed)
{
  list1 <- tetra(u=u,R=R,crit=crit, maxiter=maxiter)
  if ( list1[[2]] )
  {
    warning("TETRA() didn't converge")
    return(list(y=NULL, sigma = list1[[1]], fail = list1[[2]]))
  }
#  print("list 1 is ")
#  print(list1[[1]])
  pd <- isPosDef( list1[[1]] )
  if (pd)
  {
    if (missing(seed)){ z <- ranMVN(nRep=nReps,Sigma=list1[[1]]) }
    else { z <- ranMVN(nRep=nReps, Sigma=list1[[1]], seed) }
    cuts <- matrix(rep(qnorm(u),nReps),nReps,byrow=T)
#    print("Z is:")
#    print(z)
#    print("Cuts is:")
#    print(cuts)
    y <- ifelse(z <= cuts,1,0) 
  }
  else if (!pd)
  {
    warning("Sigma is not POSITIVE DEFINITE")
    y <- NULL
    list1[[2]] <- TRUE  # The algorithm failed since cov(Y) is not +ve definite.   
  }
  list2 <- list(y=y, sigma = list1[[1]], fail=list1[[2]])  
  return(list2)
}

