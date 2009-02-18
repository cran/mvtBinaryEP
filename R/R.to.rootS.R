`R.to.rootS` <-
function(mu, R, crit=1e-6, maxiter=20)
{
    list1 <-  tetra(u = mu, R = R, crit = crit, maxiter = maxiter)
    list1$Sigma <- u21(list1$Sigma)
    if (list1$fail)
    {
        warning("TETRA() didn't converge")
        return( list( rootSigma=NULL, sigma=NULL, pd = NA, sp = F, i=list1$locFail[1],
               j=list1$locFail[2])  )
    }
    pd <- isPosDef(list1$Sigma)
    if (pd)
    {
        return( list(rootSigma=chol(list1$Sigma), sigma=NULL, pd = T, sp = T,
                     i=list1$locFail[1], j=list1$locFail[2])  )
    }
    else
    {
        return( list(rootSigma=NULL, sigma=list1$Sigma, pd = F, sp = T,
                     i=list1$locFail[1], j=list1$locFail[2])  )
    }
}

