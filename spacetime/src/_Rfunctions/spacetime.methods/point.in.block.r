point.in.block = function( focal, dta, dist.max, n.min=100, n.max=100000 ) {

  #find all points in data dta inside of a block of distance bdist from a focal point focal 

  dlon = abs( focal[1] - dta[,1] ) 
  dlat = abs( focal[2] - dta[,2] ) 

  j = which( dlon  <= dist.max  & dlat <= dist.max  ) # faster to take a block 
  ndat = length(j)
  # print( ndat )
  if ( ndat < n.min ) return(NULL)
  
  # shrink distance until target number of cases ... assuming uniform density and compute in tems of ratio of radii: 
  dist.cur =  dist.max
  n.eps = n.max * 0.1 # small buffer
  fraction = 0.5
  stuck = 0
  while ( ndat > ( n.max + n.eps ) ) {
    ndat.good = ndat
    dist.good = dist.cur
    dist.cur = dist.good * fraction
    j.good = j
    j = which( dlon <= dist.cur  & dlat <= dist.cur ) # faster to take a block 
    ndat = length(j)
    if ( ndat == ndat.good ) stuck = stuck + 1
    if ( ndat <( n.max-n.eps)  ) {
      # overshot ... reset and use a smaller fraction
      fraction = 0.8
      dist.cur = dist.good
      ndat = ndat.good
      j = j.good
      stuck = stuck +1 
    }
    if ( stuck > 2 ) {
      fraction = fraction + 0.025 
    } 
    if (stuck > 5 ) {
      break()
    }
    print( paste( "  ... finding smaller N / distance:", ndat, dist.cur, fraction ))
  }

  out = list( 
    dist.to.nmax = dist.cur,
    indices = j,
    xypoints = dta[j,]
  )

  return(out)
}

