habitat.lookup.modeltype = function( p, sc, modtype ) {
    
  # convenience wrapper to lookup data based upon model type and do final checks upon data prior to interpolation
  require(chron)
  
  if (modtype=="time.invariant") { 
    sc = habitat.lookup(x=sc, p=p, dist.scale=p$interpolation.distances, keep.lon.lat=TRUE, datatype="time.invariant"  )
  }

  if (modtype=="complex") { 
    sc = habitat.lookup(x=sc, p=p, dist.scale=p$interpolation.distances, keep.lon.lat=TRUE, datatype="default"  )
  }

  if (modtype=="full") { 
    sc = habitat.lookup(x=sc, p=p, dist.scale=p$interpolation.distances, keep.lon.lat=TRUE, datatype="all.data"  )
  }

  return(sc)
}

