
sep.VPA.core = function(inits, ...) {
  
  # unpack parameters
    ages = V$ages
    years = V$years
    nages = length(ages)
    nyears = length(years)
    aX = c( nages-1, nages) # indices of the last 2 age classes (8 plus group and age 7)
    yX = nyears    # the index of the last year
    a = c(1:(nages-2))
    y = c(1:(nyears-1))

  F.age = inits[1:nages]
  F.year = inits[(nages+1):length(inits)]
  V$F = V$catch * NA # intialise
  V$F = F.age %*% t( F.year ) # return to real number
  V$S = cohort.analysis( V$catch, V$F, V$M )
  V$catch.pred = V$F /(V$F+V$M[,]) * V$S*(1-exp(-V$F-V$M[,]))
  V$resids = log(V$catch) - log(V$catch.pred)
  V$error = sum( V$resids^2 , na.rm=T)
  return( V )
}


