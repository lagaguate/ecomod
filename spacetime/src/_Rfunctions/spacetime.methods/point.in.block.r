point.in.block = function( focal, dta, dist.max, n.max=100, returnvalue="dist.to.nmax" ) {

  #find all points in data dta inside of a block of distance bdist from a focal point focal 

      dlon = abs( focal[1] - dta[,1] ) 
      dlat = abs( focal[2] - dta[,2] ) 

      j = which( dlon  <= dist.max  & dlat <= dist.max  ) # faster to take a block 
      ndat = length(j)
      print( ndat )
      if ( ndat < n.min ) next()
      
      # shrink distance until target number of cases ... assuming uniform density and compute in tems of ratio of radii: 
      dist.cur =  dist.max
      n.eps = n.max * 0.05 # small buffer
      fraction = 0.5
      stuck = 0
      while ( ndat > ( n.max + n.eps ) ) {
        ndat0 = ndat
        dist.cur0 = dist.cur
        dist.cur = dist.cur0 * fraction
        j = which( dlon <= dist.cur  & dlat <= dist.cur  ) # faster to take a block 
        ndat = length(j)
        if ( ndat == ndat0 ) stuck = stuck + 1
        if ( ndat <( n.max-n.eps)  ) {
          # overshot ... reset and use a smaller fraction
          fraction = 0.8
          dist.cur = dist.cur0
          ndat = ndat0
          stuck = stuck +1 
        }
       
        if ( stuck > 1 ) {
          fraction = fraction + 0.025 
          stuck = stuck +1 
        } 
        if (stuck > 6 ) break()
        print( paste( "  ... finding smaller N / distance:", ndat, dist.cur, fraction ))

      }

      # rm( dlon, dlat)
    
      out = switch( returnvalue,
        dist.to.nmax = dist.cur,
        indices = j,
        xypoints = dta[j,]
      )
 
    return(out)
}

