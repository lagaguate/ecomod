
  temperature.timeseries.interpolate = function( ip=NULL, p, g, B ) {

    if (exists( "init.files", p)) LoadFiles( p$init.files ) 
    if (exists( "libs", p)) RLibrary( p$libs ) 
    if (is.null(ip)) ip = 1:p$nruns

    # default output grid
    OP0 = expand.grid( weekno=p$wtimes, yr=p$tyears )
    OP0$fit = NA  # these will be filled in with predicted fits and se's
    OP0$se  = NA

    for ( iip in ip ) {
      mm = p$runs[iip,"loc"]
      Pi=P[mm,]
      print (mm)			
      OP = OP0
      res = NULL

      if ( p$tmethod %in% c("annual", "seasonal.basic", "seasonal.smoothed", "harmonics.1", "harmonics.2", "harmonics.3" ) ) {
        res = temperature.timeseries.interpolate.gam( p=p, g=Pi, z=OP ) 
      }
      if (p$tmethod %in% c("inla.ts.simple" ) ) {
        res = temperature.timeseries.interpolate.inla( p=p, g=Pi, z=OP ) 
      }
      if ( is.null( res) ) next()
      tbot <- attach.big.matrix( p$tbot.desc )
      tbot.se <- attach.big.matrix( p$tbot.se.desc )
      tbot[ mm,] <- res$fit
      tbot.se[mm,] <- res$se
    } # end each point
  }



