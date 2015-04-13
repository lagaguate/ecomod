

  ssa.db = function( p=NULL, DS="debug", out=NULL, tio=NULL, rn=0 ) {
    
    outdir = project.datadirectory( "model.ssa", "data", p$runname )
    odir = file.path( outdir, rn )  # rn is the default run number when not in parallel mode .. no need to change
    fn = file.path( odir, paste( "out", tio, "rdata", sep="." )) 
    
    if ( DS=="save" ) {
      if (! file.exists(odir) ) dir.create( odir, recursive=TRUE, showWarnings=FALSE  )
      save (out, file=fn, compress="xz", compression_level=2 )  ## faster 
      return (fn)
    }


    if ( DS=="load" ) {
      out = NULL
      if (file.exists( fn) ) load (fn )
      return(out)
    }
      

    if ( DS=="load.all" ) {
      out = array( NA, dim=c(p$nr, p$nc, p$n.times)  )  
      for ( i in 1:p$n.times ) {
        X = ssa.db( DS="load", tio=i )
        if ( !is.null(X) ) out[,,i] = X 
      }
      return(out)
    }
      
    
    if ( DS=="restart" ) {
      out = with(p, {
        # initiate state space with some random noise and a core area in the center of the system
        X = ssa.db( DS="load", tio=tio )
        P = RE( p, X ) 
        P.total = sum( P[] )
        list( X=X, P=P, P.total=P.total, simtime = 0, nevaluations = 0 )
      })
      return(out)
    }
     
    
    if ( DS=="debug" ) {
      out = with( p, {
        # initiate state space with some random noise and a core area in the center of the system
        X = array( 0, dim=c( nr, nc ) ) 
        rwind = floor(nr/10*4.5):floor(nr/10*5.5)
        cwind = floor(nc/10*4.5):floor(nc/10*5.5)
        X[ rwind, cwind ] = round( K[rwind, cwind ] * 0.8 )
        P = array( RE( p, X ) ,  dim=c( nr, nc, np ) ) 
        P.total = sum( P[] )
        list( X=X, P=P, P.total=P.total, simtime = 0, nevaluations = 0 )
      })
      return(out)
    }    
    
    
    if ( DS=="debug.big.matrix.rambacked" ) {
      # as in the basic "debug" form but using a bigmemory RAM object 
      # .. make sure SHM (shared memory is used an d properly configured on the OS) in /etc/fstab
      require(bigmemory)

      out = with( p, {
        X = big.matrix( nrow=nr, ncol=nc, init=0 ) 
        rwind = floor(nr/10*4.5):floor(nr/10*5.5)
        cwind = floor(nc/10*4.5):floor(nc/10*5.5)
        X[ rwind, cwind ] = round( K[rwind, cwind ] * 0.8 )
        P = big.matrix( nrow=nr, ncol=nc*np, type='double', init=RE( p, X[] )  ) 
        P.total = sum( P[] )
        bm.P = describe( P )
        bm.X = describe( X )
        list( P.total=P.total, bm.P=bm.P, bm.X=bm.X, simtime = 0, nevaluations = 0 )  
      })
      return(out)
    }
 
    
    if ( DS=="debug.big.matrix.filebacked" ) {
      # as in the basic "debug" form but using a bigmemory RAM object 
      require(bigmemory)

      out = with( p, {
        X = big.matrix( nrow=nr, ncol=nc, type="double", init=0, 
          backingpath=outdir, backingfile="ssa.X.bm.tmp", descriptorfile="ssa.X.bm.desc" ) 
        rwind = floor(nr/10*4.5):floor(nr/10*5.5)
        cwind = floor(nc/10*4.5):floor(nc/10*5.5)
        X[ rwind, cwind ] = round( K[rwind, cwind ] * 0.8 )
        P = big.matrix( nrow=nr, ncol=nc*np, type='double', init=RE( p, X[] ),
          backingpath=outdir, backingfile="ssa.P.bm.tmp", descriptorfile="ssa.P.bm.desc" ) 
        P.total = sum( P[] )
        bm.P = describe( P )
        bm.X = describe( X )
        list( P.total=P.total, bm.P=bm.P, bm.X=bm.X, simtime = 0, nevaluations = 0 )  
      }) 
      return(out)
    }


    if ( DS=="snowcrab.debug" ) {
      
      res = with( p, {
        
        # initiate state space with some random noise and a core area in the center of the system
        X = model.pde.external.db( p=p, method="snowcrab.male.mature", variable="abundance.mean" )
        X = X * 10^3
        X [ X<= eps] = 0 
# convert to integer data -- 0.5 to 1 kg ~ 1 crab fully mature; X is in t/km^2; X1000 to convert to individuals/km^2
        # X = matrix(X, sparse=TRUE)

        # add some random noise and a core area in the center of the system
        debug = FALSE
        if( debug) { 
          rwind = floor(nr/10*4.5):floor(nr/10*5.5)
          cwind = floor(nc/10*4.5):floor(nc/10*5.5)
          X[ rwind, cwind ] = round( X[ rwind, cwind ] * runif( length(X[ rwind, cwind ]) ) )
        }  
        
        X = X * runif( length(X), min=0.75, max=0.95 )  
        
        # initiate P the propensities 
        P = array( RE(p, as.matrix( X) ),  dim=c( nr, nc, np ) ) 
        P.total = sum( P[] )
        simtime = 0       # time in units of the simulation (days)
        nevaluations = 0  # used for debugging and counting evaluations to estimate computational speed ...
        return( list( X=X, P=P, P.total=P.total, simtime=simtime, nevaluations=nevaluations ) )
  
      })

      return(res)


    }


  }



