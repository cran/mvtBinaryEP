`rho.to.rootS2` <-
function(mu, p, rho, crit=1e-6, maxiter=20)
{
    tc.cor <- tetra1(c(mu,mu), rho, crit=crit, maxiter=maxiter);
    if (tc.cor[[2]])
    {
        return( list( rootSigma=NULL, sigma=NULL, pd=NA, sp=F, i=NA, j=NA ) )
    }
    pd <- as.logical( tc.cor[[1]] > ( -1/(p-1) ) )
    if (pd) { return( list( rootSigma = sqrt( tc.cor[[1]] ), sigma = NULL, pd=T, sp=T, i=NA, j=NA ) ) }
    else { return( list( rootSigma=NULL, sigma=tc.cor[[1]], pd=F, sp=T, i=NA, j=NA ) ) }
}

