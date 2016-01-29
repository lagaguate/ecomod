
  temperature.timeseries.interpolate = function( ip=NULL, p ) {

    if (exists( "init.files", p)) LoadFiles( p$init.files ) 
    if (exists( "libs", p)) RLibrary( p$libs ) 
    if (is.null(ip)) ip = 1:p$nruns

    # default output grid
    z0 = expand.grid( weekno=p$wtimes, yr=p$tyears )
    attr( z0, "out.attrs" ) = NULL
    z0$fit = NA  # these will be filled in with predicted fits and se's
    z0$se  = NA
    z0$tiyr = 2*pi* ( z0$yr + z0$weekno/52 )
    z0 = z0[ order(z0$tiyr), ]

    if ( p$tsmethod %in% c("annual", "seasonal.basic", "seasonal.smoothed", "harmonics.1", "harmonics.2", "harmonics.3" ) ) {
        interpolate.ts = temperature.timeseries.interpolate.gam
    }
    if (p$tsmethod %in% c("inla.ts.simple" ) ) {
        interpolate.ts = temperature.timeseries.interpolate.inla
    }
      
    B = hydro.db( p=p, DS="bottom.gridded.all"  )
    B$tiyr = 2*pi* ( B$yr + B$weekno/52 )
      
    # globally remove all unrealistic data  
    keep = which( B$t >= -3 & B$t <= 25 ) # hard limits
    if (length(keep) > 0 ) B = B[ keep, ]
    TR = quantile(B$t, probs=c(0.0005, 0.9995), na.rm=TRUE ) # this was -1.7, 21.8 in 2015
    keep = which( B$t >=  TR[1] & B$t <=  TR[2] )
    if (length(keep) > 0 ) B = B[ keep, ]

    P = bathymetry.db( p=p, DS="baseline" )
    tbot <- bigmemory::attach.big.matrix( p$descriptorfile.tbot  )
    tbot.se <- bigmemory::attach.big.matrix( p$descriptorfile.tbotse  )

    for ( iip in ip ) {
      mm = p$runs[iip,"loc"]
      if ( is.nan( tbot[mm,1] )) next() #  this location is problematic .. skip
      if ( !is.na( tbot[mm,1] )) next() # has a solution from previous run .. skip
      tbot[mm,1] = NaN # flag as being operated upon .. in case a restart is needed
      res = NULL
      res = try( interpolate.ts ( p=p, B=B, g=P[mm,], z=z0 ) , silent=TRUE )
      if ( class(res) %in% "try-error" ) next()
      if ( any(is.finite(res$fit)) ) {
        print (mm)			
        tbot[ mm,] <- res$fit
        tbot.se[mm,] <- res$se
      }
    } # end each point
    
    return( "completed")

      if (FALSE) {
        #debugging ..
        dm = 30
        drange = c(-1,1) * dm
        plon0 = g$plon + drange
        plat0 = g$plat + drange
        i = which( B$plon > plon0[1] & B$plon < plon0[2] & B$plat > plat0[1] & B$plat < plat0[2] )
        x = B[i,] 
        x$tiyr =  x$yr + x$weekno/52
        res$tiyr = res$yr + res$weekno/52
        plot( t~tiyr, x, xlim=range(res$tiyr), pch=20 )
        lines( fit~ tiyr, res, col="green" )
      }
  }



