point.in.block = function( focal, dta, dist.max, n.min=100, n.max=100000 ) {

  #find all points in data dta inside of a block of distance bdist from a focal point focal 

  dlon = abs( focal[1] - dta[,1] ) 
  dlat = abs( focal[2] - dta[,2] ) 

  j = which( dlon  <= dist.max  & dlat <= dist.max  ) # faster to take a block 
  ndat = length(j)
  if ( ndat < n.min ) return(NULL)
  
  # shrink distance until target number of cases ... assuming uniform density and compute in tems of ratio of radii: 
  stuck = 0
  for ( fraction in c( 1.0, 0.8, 0.6, 0.4, 0.2, 0.1 ) )  {
    dist.cur = dist.max * fraction
    j = which( dlon <= dist.cur & dlat <= dist.cur ) # faster to take a block 
    ndat = length(j)
    threshold = runif( 1, min=-0.1, max=0.1 )
    if ( ndat <= ( n.max + threshold ) ) break() 
    print( paste( "  ... finding smaller N / distance:", ndat, dist.cur, fraction ))
  }

  out = list( 
    dist.to.nmax = dist.cur,
    indices = j,
    xypoints = dta[j,]
  )

  return(out)
}

