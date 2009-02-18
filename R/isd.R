`isd` <-
function(mu, R, rho, p, crit=1e-6, maxiter=20)
{
    if (missing(mu)) { stop("The mean vector mu is a required argument") }
    
    # This corresponds to the case when an exchangeable correlation parameter is
    # specified.
    if (!missing(rho))
    {
        if (length(mu) > 1) {stop("Since rho is specified, ``mu'' must be a length 1 vector ")}
        if (rho <= -1/(p-1) | rho > 1)
        {
          stop(cat("rho must be greater than",  -1/(p-1), " and <= 1 " ) )
        }
        if (rho > -1/(p-1) & rho < 0)
        {
            rs <- rho.to.rootS(mu=mu, p=p, rho=rho, crit=1e-6, maxiter=20)
            return( list(mu=mu, rho=rho, n=p, R=NULL, rootS = rs$rootSigma, S = rs$sigma,
                     pd=rs$pd, sp=rs$sp, i=rs$i, j=rs$j) )
        }
        if (rho >= 0)
        {
            rs <- rho.to.rootS2(mu, p, rho, crit=1e-6, maxiter=20)
            return( list( mu=mu, rho=rho, n=p, R=NULL, rootS = rs$rootSigma, S = rs$sigma,
                          pd=rs$pd, sp=rs$sp, i=rs$i, j=rs$j ) )
        }

    }
    
    # This part corresponds to the case when a general correlatin parameter is
    # specified.
    if ( !missing(R) )
    {
        rs <- R.to.rootS(mu=mu, R=R, crit=crit, maxiter=maxiter)
        return( list(mu=mu, rho=NULL, n=NULL, R=R, rootS = rs$rootSigma, S = rs$Sigma,
                     pd=rs$pd, sp=rs$sp, i=rs$i, j=rs$j) )
    }
}

