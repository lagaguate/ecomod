
  temperature.timeseries.interpolate = function( ip=NULL, p, P, B ) {

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

      if ( p$tsmethod %in% c("annual", "seasonal.basic", "seasonal.smoothed", "harmonics.1", "harmonics.2", "harmonics.3" ) ) {
        res = temperature.timeseries.interpolate.gam( p=p, B=B, g=Pi, z=OP ) 
      
      }
      if (p$tsmethod %in% c("inla.ts.simple" ) ) {
        res = temperature.timeseries.interpolate.inla( p=p, B=B, g=Pi, z=OP ) 
      }

      if (FALSE) {
        #debugging ..
        dm = 2
        drange = c(-1,1) * dm
        plon0 = Pi$plon + drange
        plat0 = Pi$plat + drange
        i = which( B$plon > plon0[1] & B$plon < plon0[2] & B$plat > plat0[1] & B$plat < plat0[2] )
        x = B[i,] 
        x$tiyr =  x$yr + x$weekno/52
        OP$tiyr = OP$yr + OP$weekno/52
        plot( t~tiyr, x, pch=20 )
        lines( res$fit~ tiyr, OP, col="green" )

      }


      if ( is.null( res) ) next()
      tbot <- attach.big.matrix( p$tbot.desc )
      tbot.se <- attach.big.matrix( p$tbot.se.desc )
      tbot[ mm,] <- res$fit
      tbot.se[mm,] <- res$se
    } # end each point
  }



