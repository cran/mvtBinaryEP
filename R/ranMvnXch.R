`ranMvnXch` <-
function(rho, n, nRep=1, seed=NULL)
{
    if ( !exists(".Random.seed", envir=.GlobalEnv, inherits = FALSE) ){ runif(1) }
    if (is.null(seed))
        RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    else {
        R.seed <- get(".Random.seed", envir = .GlobalEnv)
        set.seed(seed)
        RNGstate <- structure(seed, kind = as.list(RNGkind()))
        on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }

    x = sqrt(1-rho^2)*matrix(rnorm(nRep*n),nRep)
    z = matrix( rep(rho*rnorm(nRep),n),nRep, byrow=F )
    y = x + z;

    return(y)
}

