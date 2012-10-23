

  invcompact3 = function( params, layer, K, packed ) {

    water = 1

    O = array( NA, dim=c( params["m"], params["n"] ) )
    npacked = 0
    for ( k in save.states["ksave1"]:save.states["ksave2"] ) {
      for ( i in save.states["isave1"]:save.states["isave2"] ) {
        if ( ( jc[i,k] == water ) && (nlayer[i,k] >=  layer ) ){
          npacked = npacked + 1
          O[i,k] = packed[npacked] / K
        }
      }
    }

    return(O)
  }




