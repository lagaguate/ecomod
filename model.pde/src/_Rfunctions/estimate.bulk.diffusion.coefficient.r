 
  estimate.bulk.diffusion.coefficient = function( DS="snowcrab" ) { 
 
    if (DS=="snowcrab") {
      # km^2/day :: approximation based upon D = x^2 / 2t :: slope from a plot of x^2 vs 2t:: 
      loadfunctions( "snowcrab" )
      move = get.move () #  alternate:(DS="redo")
      
      plot( dx~dt, data=move) 
    
      move$dx2 = move$dx^2
      move$dt2 = move$dt*2
      move$Da = move$dx2 / move$dt2
      move$velocity = move$dx / move$dt 
      move$log_velocity = log(move$velocity)
      hist(log(move$Da))
      hist(log(move$velocity))
        
      hist(log(move$velocity),"fd")
      mean( move$log_velocity[ which(is.finite(move$log_velocity)) ] )  # -2.729117 ( = 0.06527689 km/day) 
      var( move$log_velocity[ which(is.finite(move$log_velocity)) ] )  # 1.645147 
      
      median( move$Da, na.rm=TRUE)
      median( move$velocity, na.rm=TRUE) # 0.06907182 km / day
 

      # arithmetic mean   
      D_median = median( move$Da[ is.finite(move$Da)] )
      mean( move$Da[ is.finite(move$Da)] )

      # alternatively .. geometric mean
      plot( log(dx2) ~ dt2, data=move) 
      exp( mean( log( move$Da[ is.finite(move$Da)] ) ) )

      # with time it seems to converge towards ~ exp(0) = 1 
      plot( log(Da)~dt, move)

      #alternative approach ...
      DC =list()
      times = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200, 365, 1000 )
      for ( doff in 1:(length(times)-1) ) {
        ss = which( move$dx > times[doff] & move$dx <=times[doff+1] ) 
        if (length(ss)>3 ) {
          DC[[ doff ]] =  lm( dt*2 ~ dx^2 - 1, data=move[ss,], na.action=na.omit ) 
        }
      }   
      lapply(DC, summary)

      # Here, D estimates range from 43 (~10 day interval) to 
      # 1.8 after 800 days  --- likely dues to senescence in later and disruption initially ??? 
      # --- taking a median value for now from 7 to 16 or  10 km2/day 
      # --- more modelling might be worth it -- based upon stage and size, temperature, etc ...
      
      oy = unlist(lapply(DC, coef))
      out = cbind( times[-1], oy )
      plot( out)
      return( D_median )
    }
  
  }
