`isPosDef` <-
function(M, sym)
{
  if (!missing(sym)) { e <- eigen(M, symmetric=sym, only.values=T) }
  else { e <- eigen(M, only.values=T) }
  return( min(e[[1]]) > 0 )
}

