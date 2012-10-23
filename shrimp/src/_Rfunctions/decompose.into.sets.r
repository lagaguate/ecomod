
decompose.into.sets = function( X, threshold.depth=50 ) {
  
  z = X$minilog$depth
  ndata = length(z)

  i.not.fishing = which( z < threshold.depth ) 
  fishing = rep(1, ndata)
  fishing[ i.not.fishing ] = NA
  F = fishing * c(1:ndata)

  res = NULL
  datarange = 1
  istart = 1
  for (o in 1:100 ) {  # this is a dummy index ... there should be less than 100 profiles/file
    datarange = parse.break( istart, ndata, fishing ) 
    if ( !is.finite(datarange[1]) ) break()
    res = rbind( res, datarange )
    istart = datarange[2] + 1
  }
  X$minilog$id = NA
  for( i in 1:(nrow( res)) ) X$minilog$id [ c(res[i,1]:res[i,2]) ] = i
  X$minilog = X$minilog[ which( is.finite(  X$minilog$id )) , ]
  return( X)
  
  }


