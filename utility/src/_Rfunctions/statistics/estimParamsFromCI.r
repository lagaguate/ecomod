

    estimParamsFromCI = function( conflim, probs=c(0.025, 0.975), pm0 = c(1, 1/mean( conflim )), distribution="gamma", lower=c(1e-06, 1e-07), method="L-BFGS-B" ) {
      # conflim  is estimated/approximate 95% confidence bounds 
      # probs = associated quantiles
      # pm0 = stating guess for parameters 

      CIoptim = function( pm, conflim, probs, distribution ) {
        # theoreticalCI is theoretical probabilty function 
        # pm are the parameters of the distributional function
        theoreticalCI = switch( distribution,
          gamma = pgamma( conflim, shape=pm[1], rate = pm[2], log.p=TRUE  ), 
          normal = pnorm( conflim, mean=pm[1], sd = pm[2], log.p=TRUE )
        )
        llik = ( theoreticalCI[1] - log( probs[1] ) )^2 + ( theoreticalCI[2] - log( probs[2] ) )^2
        return( llik ) 
      }
      
      res = optim( pm0, CIoptim, conflim=conflim, probs=probs, lower=lower, method=method, distribution=distribution)
      if (res$convergence==0) {
        res = res$par
      } else {
        res= c( 0.01, 0.001)
      }

      names(res) = switch(distribution ,
        gamma = c("shape", "rate"),
        normal= c("mean", "sd")
      )
  
      return(res)

      # ----------
      debug = FALSE
      if (debug) {
        conflim = c(0.5, 10) # initial guess of 95% CI of SD
        #  E(X) = shape * scale = shape / rate = res[["shape"]] / res[["rate"]] = 3.69
      #  Var(X) = shape*scale^2 = shape / (rate^2) = res[["shape"]] / res[["rate"]] ^2 =  6.32
        qgamma( 0.025, res[["shape"]], res[["rate"]] )  # = 0.5
        qgamma( 0.975, res[["shape"]], res[["rate"]] )  # = 9.99
        curve( dgamma(x, res[["shape"]], res[["rate"]]), 0.0005, 100)
      }
    }
   
  

