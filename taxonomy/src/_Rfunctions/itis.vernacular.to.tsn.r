
  itis.vernacular.to.tsn = function( tx, itaxa=itis.db( "itaxa" )  ) {
   
    res = NA
    
    txs = unlist( strsplit( tx, "[[:space:]]+" ) )
    txs = txs[ which( nchar(txs)>1 ) ]
     
    if ( length(txs)==0 ) return ( NA )
    
    ntxs = length( txs )
    ox = c( 1:nrow(itaxa) )
    for ( ii in 1:ntxs ) {
      txs.grep = paste( "\\<", txs[ii], "\\>", sep="" )  # match whole words only
      ox = intersect( ox, grep( txs.grep , itaxa$vernacular, ignore.case=T ) ) 
    }
  
    if (length (ox) == 1) {
      res = itaxa$tsn[ox]
      res2 = itaxa$tsn_accepted[ox ]
      if ( is.finite( res2 ) ) res = res2 # if there is an accepted tsn this overrides tsn
      return( res )
    }


    # try a more general approach
    ox2 = c( 1:nrow(itaxa) )
    for ( ii in 1:ntxs ) {
      txs.grep = txs[ii]  # regex does not work with agrep 
      ox2 = intersect( ox2, agrep( txs.grep, itaxa$vernacular, ignore.case=T ) ) 
      if (length (ox2) ==1) return( ox2 )
    }
    
    ox = intersect( ox, ox2 )

    if (length( ox ) == 1) {
      res=itaxa$tsn[ox]
      res2 = itaxa$tsn_accepted[ox ]
      if ( is.finite( res2 ) ) res = res2 # if there is an accepted tsn this overrides tsn

    } else if (length( ox ) > 1) {
      res= c( itaxa$tsn[ox], itaxa$tsn_accepted[ox] )
      rr = which.max( tabulate( res ) )
      ss = sort( unique( res ) )
      res = c( rr, ss[-(which(ss==rr))] )  # first element is the best "guess" (most freq encountered)
    }

    return (res)
  }


