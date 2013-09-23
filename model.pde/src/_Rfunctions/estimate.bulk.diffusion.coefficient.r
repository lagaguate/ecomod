 
  estimate.bulk.diffusion.coefficient = function( DS="snowcrab" ) { 
 
    if (DS=="snowcrab") {
      # km^2/day :: approximation based upon D = x^2 / 2t :: slope from a plot of x^2 vs 2t:: 
      loadfunctions( "snowcrab" )
      move = get.move () #  alternate:(DS="redo")
      DC =list()
      times = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200, 365, 1000 )
      for ( doff in 1:(length(times)-1) ) {
        ss = which( move$dx > times[doff] & move$dx <=times[doff+1] ) 
        if (length(ss)>3 ) {
          DC[[ doff ]] =  lm( dt*2 ~ dx^2 - 1, data=move[ss,], na.action=na.omit ) 
        }
      }   
      lapply(DC, summary)

      # D estimates range from 43 (~10 day interval) to 
      # 1.8 after 800 days  --- likely dues to senescence in later and disruption initially ??? 
      # --- taking a median value for now from 7 to 16 or  10 km2/day 
      # --- more modelling might be worth it -- bassed upon stage and size, temperature, etc ...
      
      oy = unlist(lapply(DC, coef))
      out = cbind( times[-1], oy )
      plot( out)
      return( out )
    }
  
  }
