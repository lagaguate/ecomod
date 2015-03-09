
bottom.contact.intersect = function( sm )  {
  ## Smooth method: smooth data to remove local trends and compute first derivatives 
  
  stop( "Not working well ... need a rethink ... do not use" )

  if (0) {
    
    require( rootSolve )
    res = c(NA, NA)

    # use only the subset with data for this step
    names( sm) = c("Z", "timestamp", "ts" ) # Z is used to abstract the variable name such that any variable can be passed

    # find the row numbers which corresponds to each part of the curve, decent, bottom and ascent
    N = nrow(sm)
    Nmid = trunc(N/2)
    
    ltail = 1:Nmid
    rtail = N:Nmid

    Fn = approxfun( x=sm$ts, y=sm$Z, rule=2 )  ## extrapolate 
    #  find intersection of above with a linear function that is perpendicular to the tail profile
    
    m = grad( Fn, x=sm$ts, method="simple" ) # slopes
    mp = -1/m # slope of perpendiculars
    mp[ which(!is.finite(mp) )] = 0 
    b = sm$Z - mp * sm$ts ## intercepts
    Xrange = range( sm$ts, na.rm=TRUE )
    
    sm$n.intersections = NA
    for (i in 1:N) {
      X = sm$ts[i]
      M = mp[i]
      B = b[i]
      Hn = function( X, M, B ) { Fn(X) - (M * X + B) } 
      sm$n.intersections[i] = length( uniroot.all ( Hn, interval=Xrange, M=M, B=B, tol=1e-2, n=trunc(N/2) ) )
    }
    
    ni = modes( sm$n.intersections )
    
    for( i0 in ltail) if (sm$n.intersections[i0] > ni["simple", "ub"]) break()
    for( i1 in rtail) if (sm$n.intersections[i1] > ni["simple", "ub"]) break()

    res =  c( sm$timestamp[i0], sm$timestamp[i1] )
  }

  return(res)
}


