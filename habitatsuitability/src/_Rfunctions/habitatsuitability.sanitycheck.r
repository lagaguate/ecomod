habitatsuitability.sanitycheck = function (W, p ) {
  # constrain to certian (reasonable) bounds of some habitat variables as they are being extrapolated/interpolated 

  if (exists( "speciesofinterest", p ) ) {
    if ( p$speciesofinterest=="wolffish") {
      if (exists( "tmean.annual", W ) ) {
        o = which( W$tmean.annual > 12 )
        if (length(o) > 0) W[o,"tmean.annual"] = 12
      }
      if (exists( "tamp.annual", W ) ) {
        o = which( W$tamp.annual > 15 )
        if (length(o) > 0) W[o,"tamp.annual"] = 15
      }
      if (exists( "massTot", W ) ) {
        o = which( W$massTot > 200 )
        if (length(o) > 0) W[o,"massTot"] = 200
      }
      if (exists( "Npred", W ) ) {
        o = which( W$Npred > 120 )
        if (length(o) > 0) W[o,"Npred"] = 120
      }
      if (exists( "smr", W ) ) {
        o = which( W$smr > 0.0065 )
        if (length(o) > 0) W[o,"smr"] =0.0065
      }
    
    }

    if ( p$speciesofinterest=="wolffish") {
      if (exists( "tmean.annual", W ) ) {
      
      }
    }
    
  
  }  else { 
    
    # generic limits
    
      if (exists( "tmean.annual", W ) ) {
        o = which( W$tmean.annual > 15 )
        if (length(o) > 0) W[o,"tmean.annual"] = 15
      }
      if (exists( "tamp.annual", W ) ) {
        o = which( W$tamp.annual > 15 )
        if (length(o) > 0) W[o,"tamp.annual"] = 15
      }
   
  }

  return (W)

}

