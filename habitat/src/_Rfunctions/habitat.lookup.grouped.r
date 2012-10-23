
  habitat.lookup.grouped = function( X, p, lookuptype="all.data", sp.br=seq(5, 25, 5) ) {
    vi = c("plon", "plat", "chron")
    for ( ext in sp.br) {  # Find the best scale
			sH = habitat.lookup( X[,vi], p=p, datatype=lookuptype, dist.scale=ext )
	    if ( is.null( sH ) ) next()
      M = which( !is.finite( sH$z) )
			if ( length(M) == 0 ) break()
    }
    return( sH )
  }


