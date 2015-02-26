interpolate.xy.robust = function( xy, method, target.r2=0.9, probs=c(0.025, 0.975), loess.spans=seq( 0.2, 0.01, by=-0.01 ), inla.model="rw2", smoothing.kernel=kernel( "modified.daniell", c(2,1)), nmax=5, inla.h=0.1, inla.diagonal=0.01 ) {
  
  # simple interpolation methods
  # target.r2 == target prediction R^2

  if (FALSE) {
    x=seq(-5,5,by=0.1)
    y=sin(x) + runif(length(x))^2
    xy = data.frame( x=x, y=y )
    target.r2=0.9
    nmax=5  # max number of times to try to reduce data set
    probs=c(0.025, 0.975)
    loess.spans=seq( 0.2, 0.01, by=-0.01 )
    inla.model="rw2"
    smoothing.kernel=kernel( "modified.daniell", c(2,1)) 
  }


  if (ncol(xy) ==1 ) {
    nz = length(xy)
    z = data.frame( x=1:nz, y=xy ) 
  } else  if (ncol(xy) ==2 ) {
    nz = nrow(xy)
    z = data.frame( xy)
    names(z) = c("x", "y" )
  } else {
    stop( "Error: y or c(x,y) expected")
  }
  
  z$p= NA
  z$noise = FALSE
   
  if (method =="sequential.linear") { 
    # check adjacent values and reject if differences are greater than a given range of quantiles (95%)
      lg = 1 # lag 
      nw0 = length( which(z$noise) )
      nw = nw0 + 1  # staring with a dummy value
      while ( nw != nw0 ) {
        z$ydiff = c( diff( z$y, lag=lg ) , rep(NA,lg ))  # padding with NA's 
        yquants = quantile( z$ydiff, probs=probs, na.rm=T) 
        i = which( z$ydiff < yquants[1] | z$ydiff > yquants[2]) 
        suspect.noise =  c(i, i+lg )
        n = suspect.noise[ duplicated(suspect.noise, incomparables=c(1, nz, NA)) ] # exclude terminal points
        z$noise[ n ] = TRUE
        z$y[n] = NA  
        ii = which(is.na( z$y))
        if (length(ii)>0) z$y[ii ] = approx( x=z$x, y=z$y, xout=z$x[ii], method="linear", rule=2 )$y
        nw0 = nw
        nw = length( which(z$noise ) )
      }
    z$p[ !z$noise ] = z$y[ !z$noise ]
    z$y[n] = NA
    z$p[n] = approx( x=z$x, y=z$y, xout=z$x[n], method="linear", rule=2 )$y
  }


  if (method == "loess" ) {
    for( sp in loess.spans ) {
      # ID a good span where loess solution ~ real data
      loess.mod = try( loess( y ~ x, data=z, span=sp, control=loess.control(surface="direct") ), silent=TRUE )
      if ( "try-error" %in% class(loess.mod) ) next()
      z$p = predict( loess.mod, newdata=z ) 
      rsq = cor( z$p, z$y, use="pairwise.complete.obs" )^2
      if (!is.na(rsq)){
        if (rsq > target.r2 ) break()
      }
    }
  }


  if (method == "inla") {
    require(INLA)
    nn = abs( diff( z$x) )
    dd = median( nn[nn>0], na.rm=TRUE )
    z$xiid = z$x = jitter( z$x, amount=dd / 20 ) # add noise as inla seems unhappy with duplicates in x?
    rsq = 0
    nw = length( which(is.finite( z$y)))
    nw0 = nw + 1
    count = 0
    while ( nw != nw0 ) {
      count = count + 1
      if (count > nmax ) break() # this is CPU expensive ... try only a few times 
      v = NULL
      v = try( inla( y ~ f(xiid, model="iid", diagonal=inla.diagonal) + f(x, model=inla.model, diagonal=inla.diagonal ), data=z, 
                    control.inla=list(h=inla.h), control.predictor=list( compute=TRUE) ), silent=TRUE )
      if (!( "try-error" %in% class(v) ) ) {
        z$p = v$summary.fitted.values$mean 
        rsq = cor( z$p, z$y, use="pairwise.complete.obs" )^2
        if (!is.na(rsq)){
          if (rsq > target.r2) break()
        }
        # drop a few extreme data points and redo
        iid = v$summary.random$xiid[["mean"]]
        qiid = quantile( iid, probs=probs, na.rm=TRUE )
        i = which( iid > qiid[2] | iid < qiid[1] ) 
        if (length( i) > 0 ) {
          z$y[i] = z$p[i]
        } 
        nw0 = nw
        nw = length( which(is.finite( z$y))) # to check for convergence
      }
    }
  }
 

  if (method=="kernel.density") {
    # default method is kernel( "modified.daniell", c(2,1)), 
    # a 2X smoothing kernel: 2 adjacent values and then 1 adjacent on the smoothed series .. i.e. a high-pass filter
    nd = length(smoothing.kernel$coef) - 1 # data points will be trimmed from tails so need to recenter:
    
    # test in case x increments are not uniform
    vv = unique( diff( z$x ) )
    xr = min(vv[vv>0])  
    uu = unique( round( vv, abs(log10(xr))))
    if (length(uu) > 1 ) {
      # interpolate missing/skipped data before smoothing
      ifunc = approxfun( x= z$x, y=z$y, method="linear", rule=2 )
      r0 = range( z$x, na.rm=TRUE )
      fx = seq( r0[1], r0[2], by= min(u) ) 
      fy = ifunc( fx)
    } else if (length(uu) ==1 ) {
      fx = z$x
      fy = z$y
    }
    fn = length( fx)
    si = (nd+1):(fn-nd)
    fp = rep( NA, fn)
    fp[si] = kernapply( as.vector(z$y), smoothing.kernel )
    pfunc = approxfun( x=fx, y=fp, method="linear", rule=2)
    z$p = pfunc( z$x)
    # plot( p ~ x, z, type="l" )

    # constrain range of predicted data to the input data range
    TR =  quantile( z$y, probs=probs, na.rm=TRUE  )
    toolow = which( z$p < TR[1] )
    if ( length(toolow) > 0 )  z$p[toolow] = TR[1]
    toohigh = which( z$p > TR[2] )
    if ( length(toohigh) > 0 ) z$p[toohigh] = TR[2]
    # lines( y ~ x, z, col="green" )
  }
 
  if (method=="tukey" ) {
    z$p =smooth( z$y )
  }


  if (method=="gam") {
    require(mgcv)
    gmod = gam( y ~ s(x) , data=z)  
    z$p = predict( gmod, z, type="response" )
  }

  if (method=="smooth.spline") {
    # similar to inla .. duplicated x is problematic for smooth.spline ... add small noise   
    nn = abs( diff( z$x) )
    dd = median( nn[nn>0], na.rm=TRUE )
    z$x = jitter( z$x, amount=dd / 20) # add noise as inla seems unhappy with duplicates in x?
    rsq = 0
    nw = length( which(is.finite( z$y)))
    nw0 = nw + 1
    count = 0
    while ( nw != nw0 ) {
      count = count + 1
      if ( count > nmax ) break()  # in case of endless loop
      uu = smooth.spline( x=z$x, y=z$y, keep.data=FALSE, control.spar=list(tol=dd / 20) )
      if ( length(uu$x) != nrow(z)  ) {
        vv = approx( x=uu$x, y=uu$y, xout=z$x ) 
        z$p = vv$y
      } else {
        z$p = uu$y 
      }
      rsq = cor( z$p, z$y, use="pairwise.complete.obs" )^2
      if (!is.na(rsq)){
        if (rsq > target.r2) break()
      }
      # drop a few extreme data points and redo
      iid = z$y - z$p
      qiid = quantile( iid, probs=probs, na.rm=TRUE )
      i = which( iid > qiid[2] | iid < qiid[1] ) 
      if (length( i) > 0 ) {
        z$y[i] = z$p[i]
      } 
      nw0 = nw
      nw = length( which(is.finite( z$y))) # to check for convergence 
    }
  }

  return(z$p) 

}




