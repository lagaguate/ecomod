
    itis.lookup.exhaustive.search = function( X, vnames, vtypes, parallelrun=FALSE ) {
      
      # requires itis.tsn, tolookup as variables in X
      
      iToLookup = which( X$tolookup & !is.finite(X$itis.tsn )) 

			kingdoms = itis.db( DS="kingdoms")
			kingdoms = sort( kingdoms$kingdom_name )  
      
      # this makes sure Animalia is first .. speeds things up

      for ( k in kingdoms ) { 

				print (k)
				itaxa = itis.db( "itaxa", itis.kingdom=k )

        if ( parallelrun ) {
          
          for ( i in 1: length(vnames) ) {
            vn = vnames[i]
            vt = vtypes[i]
            lookup = intersect( iToLookup, which( !is.finite( X$itis.tsn ) ) )
            res = mclapply( lookup, itis.lookup, tx=X[,vn], itaxa=itaxa, type=vt, mc.preschedule=F  )
            X$itis.tsn[lookup] = as.numeric( unlist( res ) )
          }
    
        } else {

          for ( i in 1: length(vnames) ) {
            vn = vnames[i]
            vt = vtypes[i]
            lookup = intersect( iToLookup, which( !is.finite( X$itis.tsn ) ) )
            for ( ii in lookup ) {
              jj = itis.lookup( ii, tx=X[,vn], itaxa=itaxa, type=vt )          
              if (length(jj) == 1) {
                X$itis.tsn[ii] = jj
                print( X[ii,] )
              }
            }
          }
       } # end if
			} # end for
      
      return (X) 

    }


