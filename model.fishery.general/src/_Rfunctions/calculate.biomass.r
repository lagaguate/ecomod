
calculate.biomass = function( V ) {
  # add spawing stock biomass and biomass estimates 
  V$B = V$S * V$weight.kg[,"stock"]
  V$SSB = V$B * V$MOG[,]
  return(V)
}


