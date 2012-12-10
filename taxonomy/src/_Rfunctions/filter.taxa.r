
  filter.taxa = function ( x, method=NULL, tx=NULL, index=T ) {
		
    # default is to return the row index -- if codes are desired must use index=F
    # sequence is important here .. do not re-order unless you know what you are doing


		if ( is.null(method) || method == "alltaxa" ) {
			# do this first to keep things fast if there is nothing to do ... keep all species 
			if (is.vector(x) )     i = x 
			if (!index) i = 1:length(i)
			return(i)
		} 
		
    
    sp.codes = NULL 
    
    if ( method=="living.only" ) {
	    
      tx = taxa.db("complete")
			
      if (is.data.frame(x) ) {
        sps = taxa.specid.correct(x$spec)
      } else if (is.vector(x) ) {
        sps = taxa.specid.correct(x)
      }

      out = data.frame( spec=sps, order=1:length(x) )
			out = merge(out, tx[, c("spec", "itis.tsn", "tolookup")], by="spec", sort=FALSE )
      has.tsn = which( is.finite( out$itis.tsn ) & out$tolookup)
      sp.codes = sort( unique( out$spec[ has.tsn] ) )

    }  else {

      # this is the real core of the function, the above catch exceptions
      sp.codes = species.codes( method ) 
	  
    }

    if (is.null(sp.codes) ) stop( paste( method, "was not found .. check/modify 'species.codes'" ))
  
		if (is.data.frame(x) ) { 
			# return a filtered data frame subset **AND** with corrected species names
			x$spec = taxa.specid.correct( x$spec )  # recoding of species id's done here!
			keep = which( is.finite( x$spec ) & ( x$spec %in% sp.codes) )
			if (length(keep)>0) x = x[ keep, ]
			return (x)
		}
			
		if (is.vector(x) ) {
			# return only row indices **OR** new species codes
			# x must be a vector of species codes
			x = data.frame( spec=x, order=1:length(x) )
			x$spec = taxa.specid.correct( x$spec )  # recoding of species id's done here!
			keep = which( is.finite( x$spec ) & x$spec %in% sp.codes  )
			
			if (index) {
				return (keep)
			} else {
				x$spec.out = NA
				if (length(keep)>0) x$spec.out [keep] = x$spec[keep]
				return ( x$spec.out )	
			}
		}
	}


