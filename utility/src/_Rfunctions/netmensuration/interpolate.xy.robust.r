interpolate.xy.robust = function( xy, method, target.r2=0.9, probs=c(0.025, 0.975), loess.spans=seq( 0.2, 0.01, by=-0.01 ), inla.model="rw2" ) {
  # simple interpolation methods
  # target.r2 == target prediction R^2


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
      nw0 = 2  
      nw = nw0-1  # staring with a dummy value
      while ( nw > 0 ) {
        z$ydiff = c( diff( z$y, lag=lg ) , rep(NA,lg ))  # padding with NA's 
        yquants = quantile( z$ydiff, probs=probs, na.rm=T) 
        i = which( z$ydiff < yquants[1] | z$ydiff > yquants[2]) 
        suspect.noise =  c(i, i+lg )
        noise = suspect.noise[ duplicated(suspect.noise, incomparables=c(1, nz, NA)) ] # exclude terminal points
        nw0 = nw
        nw = length( noise )
        if (nw0==nw) break()  # converged upon a solution
        if (nw >0 ) {
          # temporarily interpolate data to permit recursion
          z$noise[noise] = TRUE
          z$y[noise ] = NA  
          ii = which(is.na( z$y))
          if (length(ii)>0) z$y[ii ] = approx( x=z$x, y=z$y, xout=z$x[ii], method="linear", rule=2 )$y
        } 
      }
    z$p[ !z$noise ] = z$y[ !z$noise ]
    z$y[noise] = NA
    z$p[noise] = approx( x=z$x, y=z$y, xout=z$x[noise], method="linear", rule=2 )$y
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
    nn = diff( z$x)
    dd = median( nn[abs(nn)>0], na.rm=TRUE )
    z$xiid = z$x = jitter( z$x, amount=dd / 20 ) # add noise as inla seems unhappy with duplicates in x?
    rsq = 0
    nw = length( which(is.finite( z$y)))
    nw0 = nw + 1
    while ( nw != nw0 ) {
      v = try( inla( y ~ f(xiid, model="iid") + f(x, model=inla.model, diagonal=.01 ), data=z, control.inla=list(h=0.05) ), silent=TRUE )
      if (!( "try-error" %in% class(v) ) ) {
        z$p = v$summary.random$x[["mean"]] 
        z$p = z$p + mean(z$y-z$p, na.rm=TRUE)
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
  
  }
 
  if (method=="tukey" ) {
    z$p =smooth( z$y )
  }


  if (method=="gam") {
  
  }

  if (method=="smooth.spline") {
    # similar to inla .. duplicated x is problematic for smooth.spline ... add small noise   
    nn = diff( z$x)
    dd = median( nn[abs(nn)>0], na.rm=TRUE )
    z$x = jitter( z$x, amount=dd / 20) # add noise as inla seems unhappy with duplicates in x?
    rsq = 0
    nw = length( which(is.finite( z$y)))
    nw0 = nw + 1
    while ( nw != nw0 ) {
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




