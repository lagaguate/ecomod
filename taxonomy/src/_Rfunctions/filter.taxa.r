
  filter.taxa = function ( x, method=NULL, tx=NULL, return.species.list=FALSE ) {
		
    # sequence is important here .. do not re-order unless you know what you are doing

    if (is.data.frame(x) ) { 
      out = x 
    } else if (is.vector(x)) {
      out = data.frame(spec=x) 
    }
		
    if ( is.null(method) || method == "alltaxa" || method=="coAll" ) {
      method = "all" # override
		} 
	
    if ( method=="living.only" ) {
      method = "all" #override

      # nothing to do yet
      # UPDate add itis checks ??? 
      #       tx = taxa.db("complete")
      #       
      #       if (is.data.frame(x) ) {
      #         sps = taxa.specid.correct(x$spec)
      #       } else if (is.vector(x) ) {
      #         sps = taxa.specid.correct(x)
      #       }
      # 
      #       out = data.frame( spec=sps, order=1:length(x) )
      #       out = merge(out, tx[, c("spec", "itis.tsn", "tolookup")], by="spec", sort=FALSE )
      #       has.tsn = which( out$tolookup )  <<< this is wrong, tolookup does not mean it is living
      #       sp.codes = sort( unique( out$spec[ has.tsn] ) )
      # 
      #     } 
      
    }
    
    # this is the real core of the function, the above catch exceptions
    sp.codes = species.codes( method )  
    if (is.null(sp.codes) ) stop( paste( method, "was not found .. check/modify 'species.codes'" ))
 
		out$spec = taxa.specid.correct( out$spec )  # recoding of species id's done here!
    keep = which( is.finite( out$spec ) & ( out$spec %in% sp.codes) )
    
    if (length(keep) == 0) {
      print (paste("No data for ", method) )
      return(NULL)
    }

    if (is.data.frame(x) ) return( out[ keep, ] )  # return the filtered data frame
 
		if (is.vector(x)) {
      # return only row indices **OR** new species codes
      if ( return.species.list ) {
				out$spec.out = NA
				out$spec.out [keep] = out$spec[keep]
				return ( out$spec.out )	
			
      } else {
				return ( keep )
			}
    } 
	}


