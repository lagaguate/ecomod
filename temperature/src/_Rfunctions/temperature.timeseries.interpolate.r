
  temperature.timeseries.interpolate = function( ip=NULL, p, P, B ) {

    if (exists( "init.files", p)) LoadFiles( p$init.files ) 
    if (exists( "libs", p)) RLibrary( p$libs ) 
    if (is.null(ip)) ip = 1:p$nruns

    # default output grid
    z0 = expand.grid( weekno=p$wtimes, yr=p$tyears )
    attr( z0, "out.attrs" ) = NULL
    z0$fit = NA  # these will be filled in with predicted fits and se's
    z0$se  = NA

    for ( iip in ip ) {
      mm = p$runs[iip,"loc"]
      g=P[mm,]
      print (mm)			
      z = z0
      res = NULL

      if ( p$tsmethod %in% c("annual", "seasonal.basic", "seasonal.smoothed", "harmonics.1", "harmonics.2", "harmonics.3" ) ) {
        res = temperature.timeseries.interpolate.gam( p=p, B=B, g=g, z=z ) 
      }
      
      if (p$tsmethod %in% c("inla.ts.simple" ) ) {
        res = temperature.timeseries.interpolate.inla( p=p, B=B, g=g, z=z ) 
      }

      if ( is.null( res) ) next()
      tbot <- attach.big.matrix( p$tbot.desc )
      tbot.se <- attach.big.matrix( p$tbot.se.desc )
      tbot[ mm,] <- res$fit
      tbot.se[mm,] <- res$se
          
      if (FALSE) {
        #debugging ..
        dm = 30
        drange = c(-1,1) * dm
        plon0 = g$plon + drange
        plat0 = g$plat + drange
        i = which( B$plon > plon0[1] & B$plon < plon0[2] & B$plat > plat0[1] & B$plat < plat0[2] )
        x = B[i,] 
        x$tiyr =  x$yr + x$weekno/52
        z$tiyr = z$yr + z$weekno/52
        plot( t~tiyr, x, pch=20 )
        lines( res$fit~ tiyr, z, col="green" )
      }

    } # end each point
  }



