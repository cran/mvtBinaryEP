`rho.to.Y` <-
function(mu, rootS, p, K=1, seed=seed)
{
    z <- ranMvnXch(rho=rootS, n=p, nRep=K, seed=seed)
    y <- ifelse( z <= qnorm(mu), 1, 0 )

    return(y)
}

