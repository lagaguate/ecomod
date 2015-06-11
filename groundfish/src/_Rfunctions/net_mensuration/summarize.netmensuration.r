
summarize.netmensuration = function( nm) {
  # find deepest point (actually modal depth)  for each set 
  # and estimate quantities for matching with gsinf
  
  oo = which( nm$depth > 10 )
  if (length( oo) == 0 ) return( NULL)  # some years have no depths ..
  x = nm [ oo, ]  # depth is a required field to determine bottom contact
  iid = sort( unique( x$nm_id ) )
  out = NULL
  for ( ii in 1:length(iid) ) {
    v = w = NULL
    w = which( x$nm_id == iid[ii] )
    v = median( w )
    if ( length(w) > 30)  {
      dd = x$depth[w]
      mm = modes( dd )
      ww = which( dd > mm$lb & dd < mm$ub )
      v = median (w[ww])
    }
    out = c( out, v )
  }
  return( nm[out,] )
}


