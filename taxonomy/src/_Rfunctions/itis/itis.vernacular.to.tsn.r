
  itis.vernacular.to.tsn = function( tx, itaxa=itis.db( "itaxa" )  ) {
   
    res = NA
    
    txs = unlist( strsplit( tx, "[[:space:]]+" ) )
    txs = txs[ which( nchar(txs)>1 ) ]
     
    if ( length(txs)==0 ) return ( NA )
    
    ntxs = length( txs )
    ix = c( 1:nrow(itaxa) )
    for ( ii in 1:ntxs ) {
      txs.grep = paste( "\\<", txs[ii], "\\>", sep="" )  # match whole words only
      ox = intersect( ix, grep( txs.grep , itaxa$vernacular, ignore.case=T ) ) 
    }
  
    if (length (ox) == 0) { 
      # try a more general approach
      txs.grep = txs[ii]  # regex does not work with agrep 
      ox = intersect( ix, agrep( txs.grep, itaxa$vernacular, ignore.case=T ) ) 
    }

    
    # ox contains candidate row indices of itaxa
    # now refine search using 
    res = itis.refine.search( ox, itaxa )
  
    if (is.data.frame( res) ) {
      res = res$tsn
    } else {
      res = NA
    }

    return (res)
  }


