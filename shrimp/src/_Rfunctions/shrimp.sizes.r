
  shrimp.sizes = function( datapath ) {

    files = list.files(path=datapath, full.names=T, recursive=T)

    res = NULL
    for (fname in files ) {
      res = rbind( res, decompose.sizes.into.sets( fname ) )
    }
    return ( res )
  }
  

