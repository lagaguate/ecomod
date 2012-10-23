

ts.collapse = function ( u ) {
  out = NULL
  for (w in sort(unique(as.numeric(as.character(u$yr))))) {
    q = u[ which(u$yr==w), ]
    mean = wtd.mean(q$mean, q$sumwgt, normwt=T, na.rm=T)
    variance = wtd.var(q$mean, q$sumwgt, normwt=T, na.rm=T) # from Hmisc
    sumweights = sum(q$sumwgt, na.rm=T)
    out = data.frame(  mean=mean, variance=variance, sumweights=sumweights, nsets=NA,
      yr=w, variable=u$variable[1], region = u$region[1], period = u$period[1]
    )
  }
  return (out)
}


