
seabird.decompose.into.sets = function( X, threshold.depth ) {
  ndata = nrow(X)
  i.not.fishing = which( X$depth < threshold.depth ) 
  X$fishing = rep(1, ndata)
  X$fishing[ i.not.fishing ] = 0
  # by taking the sum of fishing with fishing offset by 1, we can 
  # get the boundaries where fishing starts and stop by looking for 1's
  X$fishing = X$fishing + c(0, X$fishing[1:(ndata-1)] ) 
  intervals = which( fishing == 1 ) # these are the boundaries
  X$seabirdid = NA
  for (o in 1:(length(intervals)-1)) {  # this is a dummy index ... there should be less than 100 profiles/file
    intervalofinterest = intervals[o]:intervals[o+1] 
    if (length( intervalofinterest ) < 10 ) next()  # ping every 5 sec ==> 50 sec data required 
    u = sum( X$fishing[intervalofinterest] ) # min value should be 10*2 = 20
    if (u < 20) next () # not fishing, go to next interval
    X$seabirdid [ intervalofinterest ] = o
  }
  X$fishing = NULL
  return( X)
}

