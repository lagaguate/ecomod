point.in.block = function( foc, dta, dist.max, n.min=100, n.max=10000, resize=FALSE) {

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
    fractions=c( 1.1, 1.2, 1.5, 2, 2.5, 3 ) 
    for ( f in fractions )  {
        dist.cur = dist.max * f
        j = which( dlon < dist.cur & dlat < dist.cur ) # faster to take a block 
        ndat = length(j)
        print( paste( "  ... finding larger distance:", ndat, dist.cur, f ))
        if ( ndat >= n.min ) return( list( dist.to.nmax = dist.cur, indices = j, xypoints = dta[j,] ) )
    }
    return( NULL )
  }

  if ( ndat >= n.min ) {
    if ( n.max / ndat > 0.5 ) {
      # subsample to a maximum of 1/2 the data
      if ( ndat <= n.max) { 
        j = j[ .Internal( sample2( length(j), n.max )) ]  
      } else {
        j = j[ .Internal( sample(  length(j), n.max, replace=FALSE, prob=NULL)) ] 
      }
      return( list( dist.to.nmax = dist.cur, indices = j, xypoints = dta[j,] ) )
    } else {
      # reduce size of distance/block     
      fractions=c( 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.05 ) 
       for ( f in fractions )  {
          dist.cur = dist.max * f
          j = which( dlon < dist.cur & dlat < dist.cur ) # faster to take a block 
          ndat = length(j)
          if ( ndat <= n.max ) return( list( dist.to.nmax = dist.cur, indices = j, xypoints = dta[j,] ) ) 
          print( paste( "  ... finding smaller distance:", ndat, dist.cur, f ))
      }
    }
    return(NULL) 
  }
  
}



