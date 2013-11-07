

  ssa.db = function( p=NULL, ptype="debug", out=NULL, tio=NULL, rn=NULL, outdir=NULL ) {
    
    if (is.null(p)) p = list()
 
    outdir = file.path( outdir, "individual.runs", rn ) 
    
    fn = file.path( outdir, paste( "out", tio, "rdata", sep="." )) 
    
    if ( ptype=="save" ) {
      if (!is.null(out)) {
        dir.create( outdir, recursive=TRUE, showWarnings=FALSE  )
        save (out, file=fn, compress=TRUE )
        return (fn)
      }
    }


    if ( ptype=="load" ) {
        out = NULL
        if (file.exists( fn) ) load (fn )
        return(out)
    }
      

    if ( ptype=="load.all" ) {
      out = array( NA, dim=c(p$nr, p$nc, p$n.times)  )  
      for ( i in 1:p$n.times ) {
        X = ssa.db( ptype="load", tio=i )
        if ( !is.null(X) ) out[,,i] = X 
      }
      return(out)
    }
     
    
    
    if ( ptype=="restart" ) {
      p <- within( p, {
        # initiate state space with some random noise and a core area in the center of the system
        X = ssa.db( ptype="load", tio=tio )
        # initiate P the propensities 
        P = array( RE( X[], b, d, K, dr, dc ), dim=c( nr, nc, np ) )
        P.total = sum( P[] )
        nP = length( P[] )
      }) 
      return( p )
    }
     
    
    if ( ptype=="debug" ) {
     
      p <- within( p, {
  
        # initiate state space with some random noise and a core area in the center of the system
        X = array( 0, dim=c( nr, nc ) ) 
        rwind = floor(nr/10*4.5):floor(nr/10*5.5)
        cwind = floor(nc/10*4.5):floor(nc/10*5.5)
        X[ rwind, cwind ] = round( K[] * 0.8 )

        # initiate P the propensities 
        P = array( RE( X[], b, d, K, dr, dc ), dim=c( nr, nc, np ) )
        P.total = sum( P[] )
        nP = length( P[] )
      }) 
      return(p)
    }    
    
    
    
    if ( ptype=="debug.big.matrix.rambacked" ) {
      # as in the basic "debug" form but using a bigmemory RAM object 
      require(bigmemory)

      p <- within( p, {
        X[] = big.matrix( nrow=nr, ncol=nc, type="double", init=0 ) 
        rwind = floor(nr/10*4.5):floor(nr/10*5.5)
        cwind = floor(nc/10*4.5):floor(nc/10*5.5)
        X[ rwind, cwind ] = round( K[] * 0.8 )
        P = big.matrix( nrow=nr, ncol=nc*np, type='double', init=0 ) 
        P[] = array( RE( X[], b, d, K, dr, dc ), dim=c( nr, nc, np ) )
        P.total = sum( P[] )
        nP = length( P[] )
        bm.P = describe( P )
        bm.X = describe( X )
      }) 
      return(p)
    }
 
    
    if ( ptype=="debug.big.matrix.filebacked" ) {
      # as in the basic "debug" form but using a bigmemory RAM object 
      require(bigmemory)

      p <- within( p, {
        X[] = big.matrix( nrow=nr, ncol=nc, type="double", init=0, 
          backingpath=p$outdir, backingfile="ssa.X.bm.tmp", descriptorfile="ssa.X.bm.desc" ) 
        rwind = floor(nr/10*4.5):floor(nr/10*5.5)
        cwind = floor(nc/10*4.5):floor(nc/10*5.5)
        X[ rwind, cwind ] = round( K[] * 0.8 )
        P = big.matrix( nrow=nr, ncol=nc*np, type='double', init=0,
          backingpath=p$outdir, backingfile="ssa.P.bm.tmp", descriptorfile="ssa.P.bm.desc" ) 
        P[] = array( RE( X[], b, d, K, dr, dc ), dim=c( nr, nc, np ) )
        P.total = sum( P[] )
        nP = length( P[] )
        bm.P = describe( P )
        bm.X = describe( X )
      }) 
      return(p)
    }





  }



