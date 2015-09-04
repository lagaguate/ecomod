
  spacetime.plot = function( p, obj ) {
    
    if ( grep("bigmemory", obj)) {
      if ( grep("predictions", obj ) ) {
        p = spacetime.db( p=p, DS="bigmemory.inla.filenames" )
 
        pps  =  expand.grid( plons=p$plons, plats=p$plats)
        P = attach.big.matrix( p$descriptorfile.P , path=p$tmp.datadir ) 
        
        cl = 2 # default is mean value 
        if ( grep("n", obj) ) cl=1
        if ( grep("mean", obj) ) cl=2
        if ( grep("sd", obj) ) cl=3
        require(lattice)
        levelplot( log( P[,2] ) ~plons+plats, pps , 
            col.regions=rev(sequential_hcl(100)), scale=list(draw=FALSE) , aspect="iso" )
      }
    }


  }

