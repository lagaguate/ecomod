
  filter.taxa = function ( x, method=NULL, tx=NULL, index=T ) {
		# default is to return the row index -- if codes are desired must use index=F
    
		if ( is.null(method) || method == "alltaxa" ) {
			# do this first to keep things fast if there is nothing to do ... keep all species 
			if (is.vector(x) )     i = x 
			if (!index) i = 1:length(i)
			return(i)
		} 
		
    sp.codes = species.codes( method ) 
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


