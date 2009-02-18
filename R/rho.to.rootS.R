`rho.to.rootS` <-
function(mu, p, rho, crit=1e-6, maxiter=20)
{
    r.xch <- toeplitz( c( 1, rep(rho,p-1) ) )
    return( R.to.rootS(mu=mu, R=r.xch, crit=crit, maxiter=maxiter) )
}

