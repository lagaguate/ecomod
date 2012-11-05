
  filter.taxa = function ( x, method=NULL, tx=NULL, index=T ) {
		# default is to return the row index -- if codes are desired must use index=F
    
		if ( is.null(method) || method == "alltaxa" ) {
			# do this first to keep things fast if there is nothing to do ... keep all species 
			if (is.vector(x) )     i = x 
			if (!index) i = 1:length(i)
			return(i)
		} 
			
	
		if (is.data.frame(x) ) { 
			# return a filtered data frame subset **AND** with corrected species names
			x = inp
			inp$spec = taxa.specid.correct( inp$spec )  # recoding of species id's done here!
			keep = which( is.finite( inp$spec ) & inp$spec %in% species.codes( method ) )
			if (length(keep)>0) inp = inp[ keep, ]
			return (keep)
		}
			
		if (is.vector(x) ) {
			# return only row indices **OR** new species codes
			# x must be a vector of species codes
			inp = data.frame( spec=x, order=1:length(x) )
			inp$spec = taxa.specid.correct( inp$spec )  # recoding of species id's done here!
			keep = which( is.finite( inp$spec ) & inp$spec %in% species.codes( method ) )
			
			if (index) {
				return (keep)
			} else {
				inp$spec.out = NA
				if (length(keep)>0) inp$spec.out [keep] = inp$spec[keep]
				return ( inp$spec.out )	
			}
		}
	}


