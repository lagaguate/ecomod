

  itis.lookup = function( lookup, tx, itaxa, type="default" ) {
    # wrapper to output only one result to facillitate a parallel call from multicore::mclapply
    
    if (type=="default") {
      res = itis.taxa.to.tsn(  tx=tx[lookup], itaxa=itaxa )
    } else if (type=="vernacular") {
      res = itis.vernacular.to.tsn(  tx=tx[lookup], itaxa=itaxa )
    }
    
    o = NA
    if ( (length(res)==1 )) {
      if ( is.finite(res) ) {
        if ( res>0 ) {
          o=res
			}} 
		}

    return (o)
  }



