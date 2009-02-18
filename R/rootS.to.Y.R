`rootS.to.Y` <-
function(mu, rootS, K=1, seed=seed)
{
    z <- ranMVN2( nRep=K, rootS=rootS, seed=seed )
    cuts <- matrix( rep(qnorm(mu), K), K, byrow=T)
    y <- ifelse(z <= cuts, 1, 0)

    return(y)
}

