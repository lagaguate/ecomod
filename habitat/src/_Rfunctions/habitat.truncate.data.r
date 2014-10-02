  habitat.truncate.data = function( dat, varnames, probs=c(0.025, 0.975) ) {
    dr = list()
    for ( ww in varnames ) {
      dr[[ww]] = quantile( dat[,ww], probs=probs, na.rm=TRUE ) # use 95%CI
      il = which( dat[,ww] < dr[[ww]][1] )
      if ( length(il) > 0 ) dat[il,ww] = dr[[ww]][1]
      iu = which( dat[,ww] > dr[[ww]][2] )
      if ( length(iu) > 0 ) dat[iu,ww] = dr[[ww]][2]
    }
    return (dat)  
  }

  
