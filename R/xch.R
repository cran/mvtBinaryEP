`xch` <-
function(p,rho)
{
  if (p <= 0) {stop('n must be at least 1')}
  else if (p==1) {r = 1}
  else if (p > 1)
  {
    c1 = rep(rho,p-1)
    r = toeplitz(c(1,c1))
  }
  return(r)
}

