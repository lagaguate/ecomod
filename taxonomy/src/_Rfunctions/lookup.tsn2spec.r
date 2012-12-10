
  
  lookup.tsn2spec = function( tsn, tx=taxa.db(), vn="spec" ) {
    
    ### lookup is from the taxa database
    
    out = data.frame( tsn=tsn, spec=NA, name=NA )
    for ( i in 1:length(tsn) ) {
      itx = which( tx$itis.tsn == tsn[i] )
      if (length(itx) == 1 && tx$usage[itx] %in% c("accepted", "valid") ) {
        out$spec[i] = tx$spec[itx]
        out$name[i] = tx$name.scientific[itx] # for debugging
      }
    }
    return (out[, vn] )
	}


