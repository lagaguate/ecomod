

  itis.extract = function( tsn, itaxa = itis.db( "itaxa" ) ) {
    
    tx.good = NULL

    ti = which( itaxa$tsn == tsn )
    if (length( ti) != 1 ) return(NA)
    
    tx = itaxa[ ti ,]
    
    # collect potential candidate tsn's
    test = unique( na.omit(  c( tsn, tx$tsn_accepted ) ) )
    lt = length( test )
    possible.error = NA
    if ( lt == 0 ) {
      # no good candidates ... use the given tsn but raise a flag
      tsn.good = tsn
      possible.error = T
    } else if ( lt == 1 ) { 
      # good, nothing else to do
      tsn.good = test
      possible.error = F
    } else {
      # some testing required to reduce the list to the best candidate
      # criteria : ... ?
      if (!is.finite (tx$tsn_accepted[1])) {
				tsn.good = tx$tsn_accepted[1]
				possible.error = F
			} else {
				tsn.good = test[1]
	      possible.error = T
			}
    }

    ti.good = which( itaxa$tsn == tsn.good )
    tx.good = itaxa[ ti.good ,]
    tx.good$possible.error = possible.error

    if ( tsn != tsn.good ) { 
      # harvest data from original when there is missing data 
       j = which( is.na( t( tx.good )))
       tx.good[,j] = paste( tx[,j], tsn, sep=" - from - " )
    }

    return( tx.good )
  }


