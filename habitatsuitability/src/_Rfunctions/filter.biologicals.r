
filter.biologicals = function( X, ss ) {
  # take biological data --- "det" and filter them using predetermined rules 

  filter.biological = lookup.biological.filter( ss )

  if (exists("sex", filter.biological)) {
    os = which( X$sex %in% filter.biological$sex )
    if (length(os) > 0 ) X = X[ os,]
  }
    
  if (exists("mass", filter.biological)) {
    os = which( X$mass >= filter.biological$mass[1] & X$mass <= filter.biological$mass[2]  )
    if (length(os) > 0 ) X = X[ os,]
  }
  
  if (exists("len", filter.biological)) {
    os = which( X$len >= filter.biological$len[1] & X$len <= filter.biological$len[2]  )
    if (length(os) > 0 ) X = X[ os,]
  }
  
  if (exists("mat", filter.biological)) {
    os = which( X$mat %in% filter.biological$mat  )
    if (length(os) > 0 ) X = X[ os,]
  }

  return (X)

}


