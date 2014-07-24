
    presence.absence = function( X, vname, px=NULL ) {
     
      if( is.null(px)) stop("Must define px")
      # calc quantiles
      sz = which( X[,vname] == 0) # assumed to be real zeros
      si = which( X[,vname] > 0)  # positive values
      
      X$q = NA
      X$q[si] = quantile.estimate ( X[si,vname]  )
      
      s01 = which( X$q < px )  # buffer zone
      s0 = unique( c(s01, sz ) )
      s1 = which( X$q >= px )

      # after the above is set, modify for those with values known to be zero
      X$q[sz] = 1
      
      # presence-absence
      X$Y = NA
      X$Y[s1] = 1  
      X$Y[sz] = 0
      X$Y[s0] = 0

      # determine appropriate weights
      # quantiles less than p$habitat.threshold.quantile have a weight that is 1-qnt -- maximum close to zero
      X$q0 = NA
      X$q0[s1] = pmax( 0.01, X$q[s1] )
      if (length(s01)>0) X$q0[s01] = pmax( 0.01,  1 - X$q[s01] )
      X$q0[sz] = 1
      
      X$wt = X$q0  
      X$q0 =X$q = NULL
      
      return (X)  # returns/adds X$Y and X$wt to data frame which represents the P/A and relative confidence of correctness
    }


