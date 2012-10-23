
  habitat.abundance.quantiles = function( X, p ) {
  
      # determine quantiles of the postive valued data ( > 0)
      X$q = NA
      surveys = sort( unique( X$survey ) ) 
      for ( s in surveys ) {
        si = which( X$survey==s & X$abundance>0 )
        X$q[si] = quantile.estimate( X$abundance[si]  )  # convert to quantiles
        print(summary(X$q[si] ))
      }
      
      # convert from quantile to z-score 
      maxq = max( X$q[ which( X$q < 1 ) ] , na.rm=T )   
      X$q[ which(X$q==1) ] = maxq
      X$qn = qnorm( X$q )

      return(X)

  }


