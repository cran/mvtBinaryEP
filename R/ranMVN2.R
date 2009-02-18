`ranMVN2` <-
function(nRep=1, rootS, seed=NULL)
{
    if ( !exists(".Random.seed", envir=.GlobalEnv, inherits = FALSE) ){ runif(1) }
    if (is.null(seed))
        RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    else
    {
        R.seed <- get(".Random.seed", envir = .GlobalEnv)
        set.seed(seed)
        RNGstate <- structure(seed, kind = as.list(RNGkind()))
        on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }
    p <- nrow(rootS)
    Z <- matrix(rnorm(p*nRep),nRep) # Of dimension nRep by p. #
    Y <- Z %*% rootS
    return(Y)
}

