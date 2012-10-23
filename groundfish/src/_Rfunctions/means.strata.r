
means.strata = function(v, strata, w=1) {
  out = NULL
  strat = sort(unique(as.character(strata)) )
  nstrat = length(strat)

  for ( j in 1:nstrat ) {
    q = which(strata == strat[j])
    if (!is.null(q)) {
      c = cbind( strat[j], wtd.mean(v[q], w[q], normwt=T, na.rm=T), wtd.var(v[q], w[q], normwt=T, na.rm=T), sum(w[q]) )
      out = rbind (out, c)
    }
  }

  colnames(out) <- c( "strat", "mean", "var", "sumwgt" )
  out = data.frame(out)
  out$strat = as.character(out$strat)
  for (i in 2:(dim(out)[2])) {
    out[,i] = as.numeric(as.character(out[,i]))
  }

  return (out)
}



