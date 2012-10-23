
cohort.analysis = function ( catch, F, M ) { 
  ages = as.numeric(rownames(catch))
  years = as.numeric(colnames(catch))
  nages = length(ages)
  nyears = length(years)
  aX = c( nages-1, nages ) # indices of the last 2 age classes (8 plus group and age 7)
  yX = nyears    # the index of the last year
  a = c(1:(nages-2))
  y = c(1:(nyears-1))

  # seed data with terminal years and ages
  S = catch *NA # initialise
  S[aX ,] <- catch[aX,] / (F[aX,] / (F[aX,] + M[aX]) * (1 - exp( -(F[aX,] + M[aX]) )))
  S[, yX] <- catch[,yX] / (F[,yX] / (F[,yX] + M) * (1 - exp( -(F[,yX] + M) )) )
  # iteratively fill in the matrix
  for (i in 1:nages) { 
    S[a,y] <- S[a+1, y+1] * exp(M[a]) + catch[a,y] * exp(M[a]/2)  
  }
  return( S )
}


