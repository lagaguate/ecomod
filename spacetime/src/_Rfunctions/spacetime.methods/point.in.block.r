point.in.block = function( foc, dta, dist.max, n.min=100, n.max=1000, upsampling=c(1.1, 1.2), downsampling=c(0.9, 0.8), resize=FALSE) {

  #\\ find all points in data dta inside of a block of distance bdist from a focal point foc 
  #\\ col 1 = (p)lon  and  col2 = (p)lat

  dlon = abs( foc[1] - dta[,1] ) 
  dlat = abs( foc[2] - dta[,2] ) 
  j = which( dlon  <= dist.max  & dlat <= dist.max  ) # faster to take a block 
  ndat = length(j)
  dist.cur = dist.max
  if ( !resize )  return( list( dist.to.nmax = dist.cur, indices = j, xypoints = dta[j,] ) )
  if ( ndat < 5 ) return( NULL )
  if ( ndat < n.min )  {
    for ( f in upsampling )  {
        dist.cur = dist.max * f
        j = which( dlon < dist.cur & dlat < dist.cur ) # faster to take a block 
        ndat = length(j)
        print( paste( "  ... finding larger distance:", ndat, dist.cur, f ))
        if ( ndat >= n.min ) return( list( dist.to.nmax = dist.cur, indices = j, xypoints = dta[j,] ) )
    }
    return( NULL )
  }
  
  if ( ndat >= n.min ) {
    if ( ndat <= n.max * 2 ) { # if close to n.max, subsample quickly and return
      if ( ndat > n.max) { 
        j = j[ .Internal( sample(  length(j), n.max, replace=FALSE, prob=NULL)) ] 
      }
      return( list( dist.to.nmax = dist.cur, indices = j, xypoints = dta[j,] ) )
    } else {
      # reduce size of distance/block     
       for ( f in downsampling )  {
          dist.cur = dist.max * f
          j = which( dlon < dist.cur & dlat < dist.cur ) # faster to take a block 
          ndat = length(j)
          if ( ndat <= n.max ) return( list( dist.to.nmax = dist.cur, indices = j, xypoints = dta[j,] ) ) 
          print( paste( "  ... finding smaller distance:", ndat, dist.cur, f ))
      }
      # as a last resort, try sampling from the data as the distances are getting too small
      j = j[ .Internal( sample( length(j), n.max, replace=FALSE, prob=NULL)) ]
      if ( ndat <= n.max ) return( list( dist.to.nmax = dist.cur, indices = j, xypoints = dta[j,] ) ) 
    }
    return(NULL) 
  }
  
}



