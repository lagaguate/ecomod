 
    perturbation.event = function( modeltime, state, p ) {
        
      if ( p$perturbation == "fishing.random" ) {

        A = matrix(ncol=p$nc, nrow=p$nr, data=state ) # reshape vector to matrix form
        # Fishing = external.db ... ()
        
        Fishing = A * (0.5*runif(length(A)))
        A = A - Fishing

      }

      return( as.vector(state)) 
    }
    


