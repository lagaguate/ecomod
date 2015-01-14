 
temperature.timeseries.interpolate.gam = function(p, B, g, z ) {

  # harmonic method explanation:
  # old method use 1 harmonic ... forcing sinusoid as a seasonal component
  # to add an offset to a trig function (b) must add cos to a sin function
  # y ~ a + c*sin(x+b)
  # y ~ a + c*sin(b)*cos(x) + c*cos(b)*sin(x)  
  #   .. as C*sin(x+b) = C*( cos(b) * sin(x) + sin(b) * cos(x) )
  # y ~ b0 + b1*x1 + b2*x2
  # where: 
  #   a = b0
  #   c^2 = b1^2 + b2^2 = c^2*(sin^2(b) + cos^2(b))
  #   c = sqrt(b1^2 + b2^2)
  #   b1/b2 = tan(b)  
  #   b = arctan(b1/b2)

# choose model formula for GAM-based models
  mf = switch( p$tsmethod ,
    annual = ' t ~ s(yr) ',
    seasonal.basic = ' t ~ s(yr) + s(weekno, bs="cc") ', 
    seasonal.smoothed = ' t ~ s(yr, weekno) + s(yr) + s(weekno, bs="cc")  ', 
    harmonics.1 = ' t ~ s(yr) + s(yr, cos.w) + s(yr, sin.w) + s(cos.w) + s(sin.w)  ', 
    harmonics.2 = ' t ~ s(yr) + s(yr, cos.w) + s(yr, sin.w) + s(cos.w) + s(sin.w) + s(yr, cos.w2) + s(yr, sin.w2) + s(cos.w2) + s( sin.w2 ) ' , 
    harmonics.3 = ' t ~ s(yr) + s(yr, cos.w) + s(yr, sin.w) + s(cos.w) + s(sin.w) + s(yr, cos.w2) + s(yr, sin.w2) + s(cos.w2) + s( sin.w2 ) + s(yr, cos.w3) + s(yr, sin.w3)  + s(cos.w3) + s( sin.w3 ) '
  )

  mf = formula(mf)
  
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
        xi = which( x$t >= xt[1] & x$t <= xt[2] ) 
        
        if (length(xi) < p$nMin.tbot ) next()
        x = x[xi, ] 
        
        x$w = 1 / (( g$plon - x$plon)**2 + (g$plat - x$plat)**2 )# weight data in space: inverse distance squared
        x$w[ which( x$w < 1e-3 ) ] = 1e-3
        x$w[ which( x$w > 1 ) ] = 1

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
          if ( ! "try-error" %in% class( out ) ) {
            z$fit = out$fit
            z$se = out$se.fit
            break()  # candidate predictions found exit inner loop (dm)
          }
        }
      }
    } # end for dm loop						
 
    return(z)

}


