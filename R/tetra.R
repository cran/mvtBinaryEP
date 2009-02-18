`tetra` <-
function(u, R, crit=1e-6, maxiter=20)
{
  n = length(u);
  if (n == 1) { return( list( Sigma=1, fail=0, locFail=c(NA, NA) ) ) }
  sigma = R;
  for ( i in seq(1,n-1) )
  {
    for (j in seq(i+1, n) )
    {
      tcor = tetra1( c(u[i],u[j]), R[i,j] );
      if (as.logical(tcor[2]))
      {
        myList = list( Sigma=sigma, fail=tcor[2], locFail=c(i,j) )
        return(myList)
      }
      else
      {
        sigma[i,j]=tcor[1]  # upper half only, i < j #
      }
    }
  }
  myList <- list( Sigma=sigma, fail=tcor[2], locFail=c(NA, NA) )
  return(myList)
}

