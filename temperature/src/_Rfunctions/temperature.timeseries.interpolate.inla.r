 
temperature.timeseries.interpolate.inla = function(p, g, z ) {

  # clean data first
  mf = " tC ~ 0 + b0
          + f( z, model='rw1')
          + f( yr, model='ar1', param=c(1,0.0001))
          + f( pryr, model='ar1', cyclic=TRUE, param=c(1,0.0001) ) "

  
    R <- try( inla( mf, family='gaussian', data=t0, 
        control.compute=list(dic=TRUE),
        control.predictor=list( compute=TRUE),
        verbose=TRUE
    ) )
 
    ----  not finished .. 

 
    summary(R)
    plot(R)
    # plot( t ~ ti, t0 )

    for ( dm in p$dist.km ) { 
      drange = c(-1,1) * dm
      plon0 = g$plon + drange
      plat0 = g$plat + drange
      i = which( B$plon > plon0[1] & B$plon < plon0[2] & B$plat > plat0[1] & B$plat < plat0[2] )
      if (length(i) > p$nMin.tbot ) {  
        # only attempt interpolation if we have enough data (nMin.tbot)
        x = B[i,] # faster to reduce the size of B here
        # remove potentially noisy/erroneous data --- they are highly influential when there is little data 
        xt = quantile( x$t, probs=c(0.005, 0.995) )
        xi = which( x$t >= bt[1] & x$t <= bt[2] ) 
        
        if (length(xi) < p$nMin.tbot ) next()
        x = x[xi, ] 
        
        x$w = 1 / (( g$plon - x$plon)**2 + (g$plat - x$plat)**2 )# weight data in space: inverse distance squared
        x$w[ which( x$w < 1e-3 ) ] = 1e-3
        x=x[,c("t", "w", "yr", "weekno" )]

        # data transformations and creation of new variables where required for raw data 
        if ( p$tsmethod %in% c( "harmonics.1", "harmonics.2", "harmonics.3"  ) ) {
          x$tiyr =  x$yr + x$weekno/52
          x$cos.w  = cos( x$tiyr )
          x$sin.w  = sin( x$tiyr )
          z$tiyr = z$yr + z$weekno/52 
          z$cos.w  = cos( z$tiyr )
          z$sin.w  = sin( z$tiyr )
          # compute additional harmonics only if required (to try to speed things up a bit)
          if ( p$tsmethod %in% c( "harmonics.2", "harmonics.3"  ) ) {
            x$cos.w2 = cos( 2*x$tiyr )
            x$sin.w2 = sin( 2*x$tiyr )
            z$cos.w2 = cos( 2*z$tiyr )
            z$sin.w2 = sin( 2*z$tiyr )
          }
          if ( p$tsmethod %in% c( "harmonics.3"  ) ) {
            x$cos.w3 = cos( 3*x$tiyr )
            x$sin.w3 = sin( 3*x$tiyr )
            z$cos.w3 = cos( 3*z$tiyr )
            z$sin.w3 = sin( 3*z$tiyr )
          }
        }
        
        # estimate model parameters
        tsmodel = NULL 
        tsmodel = switch( p$gam.optimizer ,
          bam = try( bam( mf, data=x, weights=w ) ) ,
          bfgs = try( gam( mf, data=x, weights=w, optimizer=c("outer","bfgs")  ) ) ,
          perf = try( gam( mf, data=x, weights=w, optimizer=c("perf")  ) ) ,
          newton = try( gam( mf, data=x, weights=w, optimizer=c("outer","newton")  ) ) ,
          nlm = try( gam( mf, data=x, weights=w, optimizer=c("outer","nlm")  ) ) 
        )
        
        if ( ! "try-error" %in% class(tsmodel) ) {
          out = try( predict( tsmodel, newdata=z, type="response", se.fit=T ) ) 
          if ( ! "try-error" %in% class( out ) ) break()  # candidate predictions found exit inner loop (dm)
        }
      }
    } # end for dm loop						
 
    return(out)

}


