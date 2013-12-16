
model.pde.external.db = function( p, method="habitat", variable=NULL, filterdata=TRUE ) {
  
  # load external data on depth, and temp and Pr(habitat)
  
  plon = range (p$plons)
  plat = range (p$plats)

  out = matrix( NA, nrow=p$nr, ncol=p$nc )

  if( method=="habitat" ) {
   
    loadfunctions( "habitat")
    H = habitat.db( DS="complete", p=p, year=p$y ) 
    
    if ("avoid.poor.habitat" %in% p$parameterization ){
        H[ which( H < p$habitat.threshold.quantile ) ] = 0
    }
    
#    i = habitat.xyz.to.grid (p)  # lookuptable to map xy to matrix
  
  }

  if( method=="temperature" ) {
    loadfunctions("temperature")
    H = temperature.db( DS="climatology", p=p) 
#    i = habitat.xyz.to.grid (p)  # lookuptable to map xy to matrix
  }

  if( method=="depth" ) {
    loadfunctions("bathymetry")
    H = bathymetry.db( DS="baseline", p=p ) 
#    i = habitat.xyz.to.grid (p)  # lookuptable to map xy to matrix
  }

  if (method=="snowcrab.male.mature") { 
    loadfunctions("snowcrab")
    p$v = "R0.mass"
    H = interpolation.db( DS="interpolation.simulation.PS", p=p  )
  }

  if (method=="snowcrab.male.R1") { 
    loadfunctions("snowcrab")
    p$v = "R1.mass"
    H = interpolation.db( DS="interpolation.simulation.PS", p=p  ) 
  }

  H = H[ which( H$plon <= plon[2] & H$plon >= plon[1]), ]
  H = H[ which( H$plat <= plat[2] & H$plat >= plat[1]), ]
  
  # this is the orientation of deSolve, image, etc ..
  
  out[ cbind( round(( H$plon - plon[1]) / p$dr ) + 1, round(( H$plat - plat[1] ) / p$dc ) + 1 ) ] = H[, variable] 

  if (filterdata) {
    j = which( !is.finite( out ) | out < p$eps )
    if (length(j) > 0) out[j] = p$eps 
  }

  return (out)
    
}

