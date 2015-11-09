
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
      p$descriptorfile.S = "statistics.bigmatrix.desc"
     
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
      fn.P =  file.path( p$project.root, "data", paste( p$spatial.domain, "predictions", "rdata", sep="." ) ) 

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
        P = filebacked.big.matrix( nrow=p$nplon * p$nplat, ncol=3, type="double", init=NA, dimnames=NULL, separated=FALSE, 
          backingpath=p$tmp.datadir, backingfile=p$backingfile.P, descriptorfile=p$descriptorfile.P ) 
        return( describe(P)) 
      }


      if ( DS =="predictions.redo" ) {
        
        preds = attach.big.matrix(p$descriptorfile.P, path=p$tmp.datadir)  # predictions
        preds = preds[]
        save( preds, file=fn.P, compress=TRUE )
        return(fn.P)
      } 
    }


    # -----------------
    

    if (DS == "statistics.box")  {
      sbbox = list( plats = seq( p$corners$plat[1], p$corners$plat[2], by=p$dist.mwin ), 
                    plons = seq( p$corners$plon[1], p$corners$plon[2], by=p$dist.mwin )
      )
      return(sbbox)
    }

    # -----------------

    if (DS %in% c( "statistics", "statistics.redo", "statistics.bigmemory.initialize", "statistics.bigmemory.size"   )  ) { 
      
      # load bigmemory data objects pointers
      p = spacetime.db( p=p, DS="bigmemory.inla.filenames" )

      fn.S =  file.path( p$project.root, "data", paste( p$spatial.domain, "statistics", "rdata", sep=".") ) 

      if ( DS=="statistics" ) {
        stats = NULL
        if (file.exists( fn.S) ) load( fn.S )
        return( stats ) 
      }
 
      if ( DS=="statistics.bigmemory.initialize" ) {
        # statistics storage matrix ( aggregation window, coords )

        coords = expand.grid( p$sbbox$plons, p$sbbox$plats )
        attr( coords , "out.attrs") = NULL
        names( coords ) = c("plon", "plat")
        statsvars = c("range", "range.sd", "spatial.error", "observation.error") 
        nstats = length( statsvars ) + 2  # +2 is for coords  
        S = filebacked.big.matrix( nrow=nrow(coords), ncol=nstats, type="double", init=NA, dimnames=NULL, separated=FALSE, 
          backingpath=p$tmp.datadir, backingfile=p$backingfile.S, descriptorfile=p$descriptorfile.S ) 
        S[,1] = coords[,1]
        S[,2] = coords[,2]
        return( describe( S) ) 
      }

      if ( DS=="statistics.bigmemory.size" ) { 
        S = attach.big.matrix(p$descriptorfile.S , path=p$tmp.datadir ) 
        return( nrow(S) )
      }


      if ( DS =="statistics.redo" ) {
        #\\ spacetime.db( "statsitics.redo") .. statistics are stored at a different resolution than the final grid
        #\\   this fast (simple)-interpolates the solutions to the final grid
    
        S = attach.big.matrix(p$descriptorfile.S, path=p$tmp.datadir)  # statistical outputs
        ss = as.data.frame( S[] )
        statnames0 = c( "range", "range.sd", "spatial.var", "observation.var"  )
        statnames  = c( "range", "range.sd", "spatial.sd", "observation.sd"  )
        datalink   = c( "log", "log", "log", "log" )  # a log-link seems appropriate for these data
        names(ss) = c( "plon", "plat", statnames0 )
        rm (S)

        ss$spatial.sd = sqrt( ss$spatial.var )
        ss$observation.sd = sqrt( ss$observation.var )

        ss$spatial.var = NULL
        ss$observation.var = NULL

        # interpolate statistics where necessary and add to predictions/results: 
        locsout = expand.grid( p$plons, p$plats )
        attr( locsout , "out.attrs") = NULL
        names( locsout ) = c("plon", "plat")

        stats = matrix( NA, ncol=length(statnames), nrow=nrow( locsout) )  # output data

        range0 = median( ss$range, na.rm=TRUE )
        
        for ( ii in 1:length(statnames) ) {
          vn = statnames[ii]
          oo = which( is.finite( ss[,vn] ) & ss[,vn] > 0 )  # zero's are 
          if ( length(oo) < 30 ) next() 
          dat = ss[oo,vn]
          locs = ss[oo, c("plon", "plat") ]
          RES = spacetime.interpolate.inla.singlepass ( dat, locs, locsout, lengthscale=range0, method="fast", link=datalink[ii] )
          if ( !is.null(RES)) {
            stats[,ii] = RES$xmean
            rm( RES); gc()
          }
        }

        #    datarange = ( c( 1, 8 ))
        #    dr = seq( datarange[1], datarange[2], length.out=150)
        if (0) levelplot( log(stats[,1]) ~ plon+plat, locsout, aspect="iso", at=dr, col.regions=rev(color.code( "seis", dr))) 
        if (0) levelplot( RES$xmean ~ plon+plat, locsout, aspect="iso") 
        
        save( stats,  file=fn.S, compress=TRUE )
        return( fn.S)
      }
    } 
  }
  
