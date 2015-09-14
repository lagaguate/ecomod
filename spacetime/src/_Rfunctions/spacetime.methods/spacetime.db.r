
  spacetime.db = function( DS, p, B=NULL ) {
    #// usage: low level function to convert data into bigmemory obects to permit parallel
    #// data access and maipulation
    #// B is the xyz data to work upon
    #/+

    if (DS %in% "bigmemory.inla.filenames" ) { 

      # create file backed bigmemory objects

      p$tmp.datadir = file.path( p$project.root, "tmp" )
      if( !file.exists(p$tmp.datadir)) dir.create( p$tmp.datadir, recursive=TRUE, showWarnings=FALSE )

      # input data stored as a bigmatrix to permit operations with min memory usage
      p$backingfile.W = "input.bigmatrix.tmp"
      p$descriptorfile.W = "input.bigmatrix.desc"

      p$backingfile.P = "predictions.bigmatrix.tmp"
      p$descriptorfile.P = "predictions.bigmatrix.desc"

      p$backingfile.S = "statistics.bigmatrix.tmp"
      p$descriptorfile.S = "statstics.bigmatrix.desc"
     
#      p$backingfile.Pmat = "predictions_mat.bigmatrix.tmp"
#      p$descriptorfile.Pmat = "predictions_mat.bigmatrix.desc"
      return(p)
    }     

    
    # ------------------

    if (DS %in% c("bigmemory.inla.reset.input", "bigmemory.inla.reset.output")  ) { 
      # create file backed bigmemory objects
      
      if ( DS=="bigmemory.inla.reset.input" ) {
        spacetime.db( p=p, DS="inputdata.bigmemory.intialize", B=B )
      }
      
      if ( DS=="bigmemory.inla.reset.output" ) {  # only outputs reset ... not input bigmemory object
        spacetime.db( p=p, DS="predictions.bigmemory.initialize" )
        spacetime.db( p=p, DS="statistics.bigmemory.initialize" )
      }

      return( "complete" )
    }
   

    # ------------------

    if (DS == "inputdata.bigmemory.intialize" ) { 
      # create file backed bigmemory objects
      p = spacetime.db( p=p, DS="bigmemory.inla.filenames" )  # load bigmemory data objects pointers
      W = filebacked.big.matrix( nrow=nrow(B), ncol=ncol(B), type="double", dimnames=NULL, separated=FALSE, 
        backingpath=p$tmp.datadir, backingfile=p$backingfile.W, descriptorfile=p$descriptorfile.W ) 
      W[] = as.matrix( B[] )
      return( describe(W) )
    }
   
    # ----------------
    
    if (DS %in% "bigmemory.inla.cleanup" ) { 
      # load bigmemory data objects pointers
      p = spacetime.db( p=p, DS="bigmemory.inla.filenames" )
      todelete = file.path( p$tmp.datadir,
        c( p$backingfile.P, p$descriptorfile.P, 
           p$backingfile.S, p$descriptorfile.S, 
           p$backingfile.W, p$descriptorfile.W 
      )) 
      for (fn in todelete ) file.remove(fn) 
      return( todelete )
    }

    # -----------------

    if (DS %in% c( "predictions", "predictions.redo", "predictions.bigmemory.initialize" )  ) { 
      
      # load bigmemory data objects pointers
      p = spacetime.db( p=p, DS="bigmemory.inla.filenames" )
      fn.P =  file.path( p$project.root, "data", p$spatial.domain, "predictions.rdata" ) 

      if ( DS=="predictions" ) {
        preds = NULL
        if (file.exists( fn.P ) ) load( fn.P )
        return( preds ) 
      }

      if ( DS=="predictions.bigmemory.initialize" ) {
        # prediction indices in matrix structure 
        #  Pmat = filebacked.big.matrix( ncol=p$nplats, nrow=p$nplons, type="integer", dimnames=NULL, separated=FALSE, 
        #   backingpath=p$tmp.datadir, backingfile=p$backingfile.Pmat, descriptorfile=p$descriptorfile.Pmat ) 
        # Pmat[] = c(1:(p$nplons*p$nplats))
        # col=lat=ydir, row=lon=xdir is format of matrix image, etc
        # Pmat = matrix( 1:(p$nplons*p$nplats), ncol=p$nplats, nrow=p$nplons ) 
        # P = as.vector(Pmat)
        # Pmat[ cbind( round(( P$plon - p$plons[1]) / p$pres ) + 1, round(( P$plat - p$plats[1] ) / p$pres ) + 1 ) ] = P$var

        # predictions storage matrix (discretized) 
        P = filebacked.big.matrix( nrow=p$nplon * p$nplat, ncol=3, type="double", init=0, dimnames=NULL, separated=FALSE, 
          backingpath=p$tmp.datadir, backingfile=p$backingfile.P, descriptorfile=p$descriptorfile.P ) 
        return( describe(P)) 
      }

      if ( DS =="predictions.redo" ) {
        ppp = attach.big.matrix(p$descriptorfile.P, path=p$tmp.datadir)  # predictions

        # tidy up cases where there are no data:
        means = ppp[,2] 
        nd = which( ppp[,1]==0 )
        if (length(nd)>0) means[nd] = NA # no data .. no mean
        
        variance = ppp[,3] 
        nd = which( ppp[,1] <= 1 )
        if (length(nd)>0) variance[nd] = NA

        preds = list( 
          bbox = list( plons=p$plons, plats=p$plats ),
          m = matrix( data=means, nrow=p$nplons, ncol=p$nplats ) ,
          v = matrix( data=variance, nrow=p$nplons, ncol=p$nplats )
        )
        save( preds, file=fn.P, compress=TRUE )
        return(fn.P)
      } 
    }

    # -----------------

    if (DS %in% c( "statistics", "statistics.redo", "statistics.bigmemory.initialize", "statistics.bigmemory.size"   )  ) { 
      
      # load bigmemory data objects pointers
      p = spacetime.db( p=p, DS="bigmemory.inla.filenames" )

      fn.S =  file.path( p$project.root, "data", p$spatial.domain, "statistics.rdata" ) 

      if ( DS=="statistics" ) {
        stats = NULL
        if (file.exists( fn.S) ) load( fn.S )
        return( stats ) 
      }
 
      if ( DS=="statistics.bigmemory.initialize" ) {
        # statistics storage matrix ( aggregation window, AW )
        sbbox = list( plats = seq( p$corners$plat[1], p$corners$plat[2], by=p$dist.mwin ), 
                      plons = seq( p$corners$plon[1], p$corners$plon[2], by=p$dist.mwin )
        )
        AW = expand.grid( sbbox$plons, sbbox$plats )
        attr( AW , "out.attrs") = NULL
        names( AW ) = c("plon", "plat")
        statsvars = c("range", "range.sd", "spatial.error", "observation.error") 
        nstats = length( statsvars ) + 2  # +2 is for coords  
        S = filebacked.big.matrix( nrow=nrow(AW), ncol=nstats, type="double", init=0, dimnames=NULL, separated=FALSE, 
          backingpath=p$tmp.datadir, backingfile=p$backingfile.S, descriptorfile=p$descriptorfile.S ) 
        S[,1] = AW[,1]
        S[,2] = AW[,2]
        return( describe( S) ) 
      }

      if ( DS=="statistics.bigmemory.size" ) { 
        S = attach.big.matrix(p$descriptorfile.S , path=p$tmp.datadir ) 
        return( nrow(S) )
      }


      if ( DS =="statistics.redo" ) {
        S = attach.big.matrix(p$descriptorfile.S, path=p$tmp.datadir)  # statistical outputs
        bad = which( S[,3] == p$fail.flag )
        S[ bad, (3:ncol(S[]))] = NA

        sbbox = list( plats = seq( p$corners$plat[1], p$corners$plat[2], by=p$dist.mwin ), 
                      plons = seq( p$corners$plon[1], p$corners$plon[2], by=p$dist.mwin )
        )
        
        snr = length(sbbox$plons)
        snc = length(sbbox$plats)
        
        stats = list(
          bbox = sbbox,
          range = matrix( data=S[,1], nrow=snr, ncol=snc ) ,
          range.sd = matrix( data=S[,2], nrow=snr, ncol=snc ) ,
          var.spatial = matrix( data=S[,3], nrow=snr, ncol=snc ) ,
          var.observation = matrix( data=S[,4], nrow=snr, ncol=snc )
### add some more here ... curvature / slope, etc
        )  

        save( stats,  file=fn.S, compress=TRUE )
        return( fn.S)
      }
    } 
  }
  
