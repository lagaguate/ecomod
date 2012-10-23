
extracts.minilog.stats = function(datapath) {
  files = list.files(path=datapath, full.names=T, recursive=T)
  res = NULL
  for (fname in files ) {
    l = load.minilog.rawdata(fname)
    l = decompose.into.sets(l )
    nsets = length( unique( l$minilog$id ) ) 
    f = NULL
    for (i in 1:nsets) {
      m = l$minilog[ which( l$minilog$id == i), ]  
      o = bottom.stats( m )
      o = as.data.frame(o)
      o$setno = i
      o$filename = basename(fname)
      f = rbind( f, o )
    }
    res = rbind( res, f)
  }
  numbers = c("z", "z0", "z1", "zsd", "n", "t", "tsd", "setno")
  for (i in numbers) res[,i] = as.numeric( as.character( res[,i] ))

  dates = c("t0", "t1")
  for( i in dates) res[,i] =  as.character( res[,i] )

  chars = c("filename")
  for( i in dates) res[,i] =  as.character( res[,i] )

  return(res)
}



