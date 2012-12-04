

  itis.lookup = function( lookup, tx, itaxa, type="default" ) {
    # wrapper to output only one result to facillitate a parallel call from multicore::mclapply
    
    if (type=="default") {
      res = itis.taxa.to.tsn(  tx=tx[lookup], itaxa=itaxa )
    } else if (type=="vernacular") {
      res = itis.vernacular.to.tsn(  tx=tx[lookup], itaxa=itaxa )
    }
    return (res)
  }



