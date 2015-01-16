
bottom.contact.gating = function( Z, good, depth.min=10, depth.range=c(-30,30), depthproportion=0.5) {
  
  ## Preliminary gating: simple range limits (gating) of depths to remove real extremes
  # eliminate records with NA for depth  
  i = which(!is.finite(Z))
  if (length(i) > 0) good[i] = FALSE

  ##--------------------------------
  # eliminiate shallow records due to operation at sea level
  i = which(Z< depth.min )
  if (length(i) > 0) good[i] = FALSE

  # iz = x$ts
  # plot( Z~iz, pch=20) 
  # points( Z[good] ~ iz[good] , pch=20, col="blue" )

  ## -----
  mediandepth = median( Z[ good ], na.rm=TRUE)
  Zh = hist( Z[ good ], breaks=trunc(length(good)/3), plot =FALSE)
  modedepth = Zh$mids[ which.max( Zh$counts ) ]
  depth.bottom = max( mediandepth, modedepth, na.rm=TRUE)


  ##--------------------------------
  # eliminiate records that are shallower than a given percentage of the median depth
  i = which(  Z< (depthproportion * depth.bottom ) )
  if (length(i) > 0) good[i] = FALSE
  # points( Z[good] ~ iz[good] , pch=20, col="green" )


  ##--------------------------------
  ## Filtering based on median depth

  depth.diff =  Z - depth.bottom 
  i = which( depth.diff < depth.range[1] | depth.diff> depth.range[2])
  if (length(i) > 0) good[i] = FALSE
  # points( Z[good] ~ iz[good] , pch=20, col="cyan" )

  return(good)

}


