
  habitat.lookup.simple = function( X,  p, vnames="", lookuptype="", sp.br=c(2,4,8,16,32,64) ) {

    nl = length( vnames)
    for ( ii in 1:nl ) {
      v = vnames[ii]
      vl = lookuptype[ii]
      vi = c("plon", "plat", "chron")
      if (vl %in% c("substrate", "depth") ) vi=c("plon", "plat")

      for ( ext in sp.br) {
				M = which( !is.finite(X[,v]) )
				if ( length(M) == 0 ) break()
			  res =  habitat.lookup( X[M,vi], p=p, datatype=vl, dist.scale=ext )
        if ( !is.null( res )) X[M,v] = res
			}
    }
    
    return( X[, vnames] )
  }

