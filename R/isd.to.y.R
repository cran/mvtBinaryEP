`isd.to.y` <-
function(isd, nRep, crit=1e-6, maxiter=20, seed=NULL)
{
    mu <- isd$mu
    rho <- isd$rho
    rootS <- isd$rootS
    p <- isd$n

    if( !is.null(rootS) & !is.null(rho) )
    {
        if (rho >= 0) { return(rho.to.Y(mu=mu, rootS=rootS, p=p, K=nRep, seed=seed)) }
    }
    if ( !is.null(rootS) )
    {
        return(rootS.to.Y(mu=mu, rootS, K=nRep, seed=seed))
    }
    else
    {
        y <- NULL
        return(y)
    }
}

