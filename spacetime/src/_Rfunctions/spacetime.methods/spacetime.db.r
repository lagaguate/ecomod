
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
      p$backingfile.Y = "input.Y.bigmatrix.tmp"
      p$descriptorfile.Y = "input.Y.bigmatrix.desc"

      p$backingfile.X = "input.X.bigmatrix.tmp"
      p$descriptorfile.X = "input.X.bigmatrix.desc"

      p$backingfile.LOCS = "input.LOCS.bigmatrix.tmp"
      p$descriptorfile.LOCS = "input.LOCS.bigmatrix.desc"

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

      nr = nrow(B)
      
      fn.Y = file.path(p$tmp.datadir, p$backingfile.Y )
      fn.X = file.path(p$tmp.datadir, p$backingfile.X )
      fn.LOC = file.path(p$tmp.datadir, p$backingfile.LOC )
      if ( file.exists( fn.Y) ) file.remove( fn.Y) 
      if ( file.exists( fn.X) ) file.remove( fn.X) 
      if ( file.exists( fn.LOC) ) file.remove( fn.LOC) 

 
      # dependent variable
      Y = filebacked.big.matrix( nrow=nr, ncol=1, type="double", dimnames=NULL, separated=FALSE, 
        backingpath=p$tmp.datadir, backingfile=p$backingfile.Y, descriptorfile=p$descriptorfile.Y )
      if ( "data.frame" %in% class(B) ) {
        Y[] = as.matrix( B[ , p$variables$Y ] )
      } else if ( "SpatialGridDataFrame" %in% class(B) ) {
        Y[] = as.matrix( slot(B, "data")[, p$variables$Y ]  )
      }

      # independent variables/ covariates
      if ( !is.null(  p$variables$X ) ) {
        nc = length( p$variables$X )
        X = filebacked.big.matrix( nrow=nr, ncol=nc, type="double", dimnames=NULL, separated=FALSE, 
          backingpath=p$tmp.datadir, backingfile=p$backingfile.X, descriptorfile=p$descriptorfile.X ) 
        if ( "data.frame" %in% class(B) ) {
          X[] = as.matrix( B[ , p$variables$X ] )
        } else if ( "SpatialGridDataFrame" %in% class(B) ) {
          X[] = as.matrix( slot(B, "data")[, p$variables$X ]  )
        }
      }        
      
      # coordinates
      LOCS = filebacked.big.matrix( nrow=nr, ncol=2, type="double", dimnames=NULL, separated=FALSE, 
          backingpath=p$tmp.datadir, backingfile=p$backingfile.LOCS, descriptorfile=p$descriptorfile.LOCS ) 
      if ( "data.frame" %in% class(B) ) {
        LOCS[] = as.matrix( B[ , p$variables$X ] )
      } else if ( "SpatialGridDataFrame" %in% class(B) ) {
        LOCS[] = as.matrix( coordinates(B) )
      }
      
      return( "complete" )
    }
   
    # ----------------
    
    if (DS %in% "bigmemory.inla.cleanup" ) { 
      # load bigmemory data objects pointers
      p = spacetime.db( p=p, DS="bigmemory.inla.filenames" )
      todelete = file.path( p$tmp.datadir,
        c( p$backingfile.P, p$descriptorfile.P, 
           p$backingfile.S, p$descriptorfile.S, 
           p$backingfile.Y, p$descriptorfile.Y, 
           p$backingfile.X, p$descriptorfile.X, 
           p$backingfile.LOCS, p$descriptorfile.LOCS 
      )) 
      for (fn in todelete ) file.remove(fn) 
      return( todelete )
    }

    # -----------------

    if (DS %in% c( "predictions", "predictions.redo", "predictions.bigmemory.initialize" )  ) { 
      # load bigmemory data objects pointers
      p = spacetime.db( p=p, DS="bigmemory.inla.filenames" )
      rootdir = file.path( p$project.root, "interpolated" )
      dir.create( rootdir, showWarnings=FALSE, recursive =TRUE) 
      fn.P =  file.path( rootdir, paste( "spacetime", "predictions", p$spatial.domain, "rdata", sep=".") ) 
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

    if (DS %in% c( "statistics", "statistics.redo", "statistics.bigmemory.initialize", 
                   "statistics.bigmemory.size" , "statistics.bigmemory.status"  )  ) { 
      
      # load bigmemory data objects pointers
      p = spacetime.db( p=p, DS="bigmemory.inla.filenames" )
      rootdir = file.path( p$project.root, "interpolated" )
      dir.create( rootdir, showWarnings=FALSE, recursive =TRUE) 
      fn.S =  file.path( rootdir, paste( "spacetime", "statistics", p$spatial.domain, "rdata", sep=".") ) 

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

      if ( DS=="statistics.bigmemory.status" ) { 
        S = attach.big.matrix(p$descriptorfile.S , path=p$tmp.datadir )
        # problematic and/or no data (i.e., land) and skipped
        i = which( is.nan( S[,3] ) )
     
        # not yet completed
        j = which( is.na( S[,3] ) ) 

        # completed 
        k = which( is.finite (S[,3])  ) # not yet done

        return( list(problematic=i, incomplete=j, completed=k, n.total=nrow(S[]), 
                     n.incomplete=length(j), n.problematic=length(i), n.complete=length(k)) ) 
      }

      if ( DS =="statistics.redo" ) {
        #\\ spacetime.db( "statsitics.redo") .. statistics are stored at a different resolution than the final grid
        #\\   this fast interpolates the solutions to the final grid
        p = spacetime.db( p=p, DS="bigmemory.inla.filenames" )
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
        
        # trim quaniles in case of extreme values
        for ( v in statnames ) {
          vq = quantile( ss[,v], probs= c(0.025, 0.975), na.rm=TRUE )
          ii = which( ss[,v] < vq[1] )
          if ( length(ii)>0) ss[ii,v] = vq[1]
          jj = which( ss[,v] > vq[2] )
          if ( length(jj)>0) ss[jj,v] = vq[2]
        }

        locsout = expand.grid( p$plons, p$plats )
        attr( locsout , "out.attrs") = NULL
        names( locsout ) = c("plon", "plat")

        stats = matrix( NA, ncol=length(statnames), nrow=nrow( locsout) )  # output data
        colnames(stats)=statnames

        for ( iv in 1:length(statnames) ) {
          vn = statnames[iv]
          # create a "surface" and interpolate to larger grid using
          # (gaussian) kernel-based smooth on the log-scale
          z = log( matrix( ss[,vn], nrow=length(p$sbbox$plons), ncol=length( p$sbbox$plats) ) )
          RES = NULL
          RES = spacetime.interpolate.kernel.density( x=p$sbbox$plons, y=p$sbbox$plats, z=z, 
            locsout=locsout,  nxout=length(p$plons), nyout=length( p$plats),
            theta=p$dist.mwin, xwidth=p$dist.mwin*10, ywidth=p$dist.mwin*10 ) # 10 SD of the normal kernel
          # 10 SD of the normal kernel
          if ( !is.null( RES )) stats[,iv] = exp( RES$z ) # return to correct scale

          method = FALSE
          if (method=="inla.fast") { # fast, but not fast enough for prime time yet
            # interpolation using inla is also an option 
            # but will require a little more tweaking as it was a bit slow
            range0 = median( ss$range, na.rm=TRUE )
            oo = which( is.finite( ss[,vn] ) ) 
            if ( length(oo) < 30 ) next() 
            RES = spacetime.interpolate.inla.singlepass ( 
              ss[oo,vn], ss[oo, c("plon", "plat") ], locsout, 
              lengthscale=range0, method="fast", link=datalink[iv] )
            if ( !is.null( RES )) stats[,iv] = RES$xmean
            rm (RES); gc()
          }          
        }        
        
        save( stats,  file=fn.S, compress=TRUE )
        return( fn.S)

        plotdata=FALSE ## to debug
        if (plotdata) {
          p$spatial.domain="canada.east"  # force isobaths to work in levelplot
          datarange = log( c( 5, 1200 ))
          dr = seq( datarange[1], datarange[2], length.out=150)
          oc = landmask( db="worldHires", regions=c("Canada", "US"), 
                         return.value="not.land", tag="predictions" )  ## resolution of "predictions" which is the final grid size
          toplot = cbind( locsout, z=(stats[,"range"]) )[oc,]
          resol = c(p$dist.mwin,p$dist.mwin)
          levelplot( log(z) ~ plon + plat, toplot, aspect="iso", at=dr, col.regions=color.code( "seis", dr) ,
            contour=FALSE, labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE), cex=2, resol=resol,
            panel = function(x, y, subscripts, ...) {
              panel.levelplot (x, y, subscripts, aspect="iso", rez=resol, ...)
              cl = landmask( return.value="coast.lonlat",  ylim=c(36,53), xlim=c(-72,-45) )
              cl = lonlat2planar( data.frame( cbind(lon=cl$x, lat=cl$y)), proj.type=p$internal.crs )
              panel.xyplot( cl$plon, cl$plat, col = "black", type="l", lwd=0.8 )
            }
          ) 
          p$spatial.domain="canada.east.highres"
        }
      }
    } 
  }
  
