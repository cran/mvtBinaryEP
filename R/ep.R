`ep` <-
function(mu, R, rho, n, isd=NULL, nRep=1, seed=NULL, crit=1e-6, maxiter=20)
{
    if (nRep < 1   ) stop("Number of replications must be at least 1") 
    if (crit <= 0  ) stop("Precision criterion must be greater than zero") 
    if (maxiter < 1) stop("Maximum number of iterations must be at least 1") 
    
    if ( missing(mu) )
    {
        # If ISD is present, then use it!
        if ( !is.null(isd) )
        {
            #Making default response names.
            if ( !is.null(isd$n) ) {lgth <- n }
            else {lgth <- length( isd$mu) }
            cnames <- rep(NA, lgth)
            for (i in 1:lgth){ cnames[i] <- paste("y", i, sep="") }
            
            y <- matrix( isd.to.y(isd=isd, nRep=nRep, crit=crit, maxiter=maxiter, seed=seed),ncol=length(isd$mu) )
            rownames(y) <- 1:nrow(y)
            colnames(y) <- cnames
            return( list(y=data.frame(y), isd=isd) )
        }
        else {stop("mu is a required argument")}
    }
    
    # BEGIN: generating default response names .
    if ( !missing(n) ) { m <- n }
    else { m <- length(mu) }
    cnames <- rep(NA, m)
    for(i in 1:m)
    {
        cnames[i] <- paste("y", i, sep="")
    }
    # END: generating default response names .
    
    
    if  ( (any(mu <  0) ) | (any(mu > 1 ) ) ) stop("The vector `mu' is out of range or degenerate")
    if  ( (any(mu == 0) ) | (any(mu == 1) ) ) warning("At least one member of mu is degenerate (0 or 1)")

    # If rho is specified, then use it!
    if ( !missing(rho) )
    {
        if((rho > 1) | (rho < -1)) { stop("Your correlation must adhere to -1 <= rho <= 1 ") }
        if (missing(n)) {stop("The cluster size ``n'' is needed")}
        if (length(mu) > 1){stop("``mu'' must be a vector of length 1")}

        isd1 <- isd( mu=mu, rho=rho, p=n, crit=crit, maxiter=maxiter )
        y <- isd.to.y(isd=isd1, nRep=nRep, crit=crit, maxiter=maxiter, seed=seed)
        rownames(y) <- 1:nrow(y)
        colnames(y) <- cnames

        return(list(y=y, isd=isd1))
    }

    # If R is specified, then use it!
    if (!missing(R))
    {
        isd1 <- isd( mu=mu, R=R, crit=crit, maxiter=maxiter )
        y <- isd.to.y(isd=isd1, nRep=nRep, crit=crit, maxiter=maxiter, seed=seed)

        rownames(y) <- 1:nrow(y)
        colnames(y) <- cnames

        return(list(y=y, isd=isd1))
    }
    else
    {
        stop("You need to provide either `rho', `R', or `isd' ")
    }
}
