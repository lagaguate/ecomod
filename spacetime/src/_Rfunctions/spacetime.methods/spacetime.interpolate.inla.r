 
  spacetime.interpolate.inla = function( ip=NULL, p, debugrun=FALSE ) {
    #\\ generic spatial and space-time interpolator using inla
    #\\ parameter and data requirements can be seen in bathymetry\src\bathymetry.r
    #\\ note this can run in parallel and serial mode
    
    # ip is the first parameter passed in the parallel mode
    if (exists( "init.files", p)) LoadFiles( p$init.files ) 
    if (exists( "libs", p)) RLibrary( p$libs ) 
    if (is.null(ip)) {
      if( exists( "nruns", p ) ) {
        ip = 1:p$nruns  
      }
    }
   
    # load bigmemory data objects pointers
    p = spacetime.db( p=p, DS="bigmemory.inla.filenames" )
 
    options(bigmemory.allow.dimnames=TRUE)
  
    # data file definitions
    Y = attach.big.matrix(p$descriptorfile.Y, path=p$tmp.datadir )  # input data -- dependent vars
    colnames(Y) = p$variables$Y
    hasdata = 1:length(Y) 
    bad = which( !is.finite( Y[])) 
    if (length(bad)> 0 ) hasdata[bad] = NA
    
    if ( p$variables$X != "none" ) {
      X = attach.big.matrix(p$descriptorfile.X, path=p$tmp.datadir )  # input data  -- independent vars
      if ( ncol(X)==1 ) {
        bad = which( !is.finite( X[]) ) 
      } else {
        colnames(X) = p$variables$X
        bad = which( !is.finite( rowSums(X[])) ) 
      }
      if (length(bad)> 0 ) hasdata[bad] = NA
    }

    LOCS = attach.big.matrix(p$descriptorfile.LOCS, path=p$tmp.datadir )  # input data -- location vars
    colnames(LOCS) = p$variables$LOCS
    bad = which( !is.finite( rowSums(LOCS[]))) 
    if (length(bad)> 0 ) hasdata[bad] = NA

    H = na.omit( hasdata )
    rm( hasdata); gc()
    
    # storage of indices  .. keep here :: more memory usage but fewer operations
    Pmat = matrix( 1:(p$nplons*p$nplats), ncol=p$nplats, nrow=p$nplons, byrow=T )  
    P = attach.big.matrix(p$descriptorfile.P , path=p$tmp.datadir )  # predictions
    S = attach.big.matrix(p$descriptorfile.S , path=p$tmp.datadir )  # statistical outputs

    # priors 
    kappa0 = sqrt(8)/p$expected.range
    tau0 = 1/(sqrt(4*pi)*kappa0* p$spacetime.link( p$expected.sigma) )

    for ( iip in ip ) {
      dd = p$runs[ iip, "jj" ]
      # dd=26025
      if ( debugrun) cat( paste(  Sys.time(), Sys.info()["nodename"], "index=", dd,  "start \n" ), file=p$debug.file, append=TRUE ) 
    
      focal = t(S[dd,])
      if ( is.nan( focal[3] ) ) next()
      if ( !is.na( focal[3] ) ) next()
      
      S[dd,3] = NaN   # this is a flag such that if a run fails (e.g. in mesh generation), it does not get revisited
      # .. it gets over-written below if successful

      # choose a distance <= p$dist.max where n is within range of reasonable limits to permit a numerical solution  
      # slow ... need to find a faster solution
      ppp = NULL
      ppp = try( point.in.block( focal[1,c(1,2)], LOCS, dist.max=p$dist.max, n.min=p$n.min, n.max=p$n.max, resize=TRUE ) )
      if( is.null(ppp)) next()
      if (class( ppp ) %in% "try-error" ) next()
    
      dist.cur = ppp$dist.to.nmax
      j = intersect( H, ppp$indices )
      rm(ppp)

      ndata = length(j)
      if (ndata < p$n.min) next()

      if ( debugrun)  {
        cat( paste(  Sys.time(), Sys.info()["nodename"], "index=", dd, "n=", ndata, "dist=", dist.cur, "\n" ), file=p$debug.file, append=TRUE ) 
      }

      # prediction locations
      doff = p$inla.mesh.offset * (dist.cur/p$dist.max) # scale to dist.max
      mbuffer = sum( doff )
      ppdist =  dist.cur - mbuffer 
      if (ppdist < min(p$inla.mesh.max.edge)*5) ppdist = min(p$inla.mesh.max.edge)*5

      preds.diffs = seq( from=-ppdist, to=ppdist, by=p$pres )
      preds.diffs = preds.diffs[ which( preds.diffs >= -dist.cur &  preds.diffs <= dist.cur ) ]
   
      pa_plons = focal[1] + preds.diffs
      pa_plats = focal[2] + preds.diffs

      # range checks.. cc=clipped
      pa_plons_cc = pa_plons[ which( pa_plons >= min(p$plons) & pa_plons <= max(p$plons) ) ]
      pa_plats_cc = pa_plats[ which( pa_plats >= min(p$plats) & pa_plats <= max(p$plats) ) ]
      
      pa = expand.grid( plon=pa_plons_cc, plat=pa_plats_cc ) # coords of clipped prediction area
      attr( pa, "out.attrs") = NULL
      pm_row = round(( pa$plon - p$plons[1]) / p$pres ) + 1
      pm_col = round(( pa$plat - p$plats[1]) / p$pres ) + 1  

      pa$i = Pmat[ cbind(pm_row, pm_col) ]

      rm( pm_row, pm_col, pa_plons_cc, pa_plats_cc, pa_plons, pa_plats ) ; gc()

      if ( debugrun)  {
        cat( paste(  Sys.time(), Sys.info()["nodename"], "index=", dd, " \n" ), file=p$debug.file, append=TRUE ) 
      }

      locs_noise = runif( ndata*2, min=-p$pres*p$spacetime.noise, max=p$pres*p$spacetime.noise ) # add  noise  to prevent a race condition

      # also sending direct distances rather than proportion seems to cause issues.. 
      MESH = spacetime.mesh( locs=LOCS[j,]+locs_noise, 
        lengthscale=dist.cur*2, 
        max.edge=p$inla.mesh.max.edge * dist.cur*2, 
        bnd.offset=p$inla.mesh.offset, 
        cutoff=p$inla.mesh.cutoff, 
        convex=p$inla.mesh.hull.radius, 
        resolution=p$inla.mesh.hull.resolution )
 
      if ( is.null(MESH)) {
        cat( paste(  Sys.time(), Sys.info()["nodename"], "index=", dd,  "mesh try error \n" ), file=p$debug.file, append=TRUE ) 
        next()
      }

      if ( debugrun) {
        cat( paste(  Sys.time(), Sys.info()["nodename"], "index=", dd,  "mesh finished \n" ), file=p$debug.file, append=TRUE ) 
        plot( MESH )  # or ... spacetime.plot( p=p, "mesh", MESH=MESH )
      }

      SPDE = inla.spde2.matern( MESH,      
        alpha=p$inla.alpha, # alpha is the Bessel smoothness factor .. 1(?) gives exponential correlation function
        B.tau=matrix(c(log(tau0),-1,+1),nrow=1,ncol=3),
        B.kappa=matrix(c(log(kappa0),0,-1),nrow=1,ncol=3),
        theta.prior.mean=c(0,0), # thetas are the internal representations of prior offsets for tau and kappa (i.e.,range and sigma)
        theta.prior.prec=c(0.1, 0.1) # precision of thetas
      ) 

      # effects .. a list of two elements: first is for SPDE and second is for covariates
      obs_index = inla.spde.make.index(name=p$spatial.field.name, SPDE$n.spde)
      obs_eff = list()
      obs_eff[["spde"]] = c( obs_index, list(intercept=1) )
      if ( p$variables$X != "none" ) {
        if ( ncol(X)==1 ) {
          obs_eff[["covar"]] = as.list( as.data.frame( X[j])) # bigmatrix quirk
        } else {
          obs_eff[["covar"]] = as.list( as.data.frame( X[j,]))
        }
        obs_A = list( inla.spde.make.A( mesh=MESH, loc=LOCS[j,] ), 1 )
      } else {
        obs_A = list( inla.spde.make.A( mesh=MESH, loc=LOCS[j,] ) ) # no effects
      }
      obs_ydata = list()
      obs_ydata[[ p$variables$Y ]] = p$spacetime.link ( Y[j] )
      DATA = inla.stack( tag="obs", data=obs_ydata, A=obs_A, effects=obs_eff, remove.unused=FALSE ) 
      rm ( obs_index, obs_eff, obs_ydata, obs_A )
      # remove.unused=FALSE ensures that we can look at the estimated field effect without having to do expensive separate predictions.
      # DATA$A is projection matrix to translate from mesh nodes to data nodes

      # prediction stack
      if ( any( grepl ("predictions.direct", p$spacetime.outputs))) {
        preds_locs = as.matrix( LOCS[ j, ])
        preds_index = inla.spde.make.index( name=p$spatial.field.name, SPDE$n.spde)
        preds_eff = list()
        preds_eff[["spde"]] = c( preds_index, list(intercept=1) )
        if ( p$variables$X != "none" ) {
          if ( ncol(X)==1 ) {
            preds_eff[["covar"]] = as.list(as.data.frame(X[ j ])) 
          } else {
            preds_eff[["covar"]] = as.list(as.data.frame(X[ j ,])) 
          }
          preds_A = list( inla.spde.make.A(MESH, loc=preds_locs ), 1)
        } else {
          preds_A = list( inla.spde.make.A(MESH, loc=preds_locs ) )
        }
        preds_ydata = list()
        preds_ydata[[ p$variables$Y ]] = NA ## ie. to predict
        PREDS = inla.stack( tag="preds", data=preds_ydata, A=preds_A, effects=preds_eff, remove.unused=FALSE )
        DATA = inla.stack(DATA, PREDS )
        preds_stack_index = inla.stack.index( DATA, "preds")$data  # indices of predictions in stacked data
        rm ( preds_eff, preds_ydata, preds_A, PREDS, preds_index, preds_locs )
      }
     
      RES = NULL
      RES = spacetime.inla.call( FM=p$modelformula, DATA=DATA, SPDE=SPDE, FAMILY=p$spacetime.family )

      if (is.null(RES)) {
        cat( paste(  Sys.time(), Sys.info()["nodename"], "index=", dd,  "inla call error \n" ), 
            file=p$debug.file, append=TRUE ) 
        # inla.spde2.matern creates files to disk that are not cleaned up:
        spdetmpfn = SPDE$f$spde2.prefix
        fns = list.files( dirname( spdetmpfn ), all.files=TRUE, full.names=TRUE, recursive=TRUE, include.dirs=TRUE )  
        oo = grep( basename(spdetmpfn), fns )
        if(length(oo)>0) file.remove( sort(fns[oo], decreasing=TRUE) )
        next()
      }

      if ( debugrun) {
        cat( paste(  Sys.time(), Sys.info()["nodename"], "index=", dd,  "computations finished \n" ), 
            file=p$debug.file, append=TRUE ) 
        print(RES)
        print( summary(RES)) 
        # low level debugging .. and looking at posterior marginals
        idat =  inla.stack.index( DATA, 'data')$data # indices of data locations
        spacetime.plot( p=p, "range", RES=RES, MESH=MESH, SPDE=SPDE, vname=p$spatial.field.name, idat=idat )
        spacetime.plot( p=p, "nugget", RES=RES, MESH=MESH, SPDE=SPDE, vname=p$spatial.field.name, idat=idat )
        spacetime.plot( p=p, "partial.sill", RES=RES, MESH=MESH, SPDE=SPDE, vname=p$spatial.field.name, idat=idat )
        spacetime.plot( p=p, "fixed.intercept", RES=RES, MESH=MESH, SPDE=SPDE, vname=p$spatial.field.name, idat=idat )
        rm (idat)
      }
       
      # inla.spde2.matern creates files to disk that are not cleaned up:
      spdetmpfn = SPDE$f$spde2.prefix
      fns = list.files( dirname( spdetmpfn ), all.files=TRUE, full.names=TRUE, recursive=TRUE, include.dirs=TRUE )  
      oo = grep( basename(spdetmpfn), fns )
      if(length(oo)>0) file.remove( sort(fns[oo], decreasing=TRUE) )
      
      rm( DATA ); gc() 

      # ----------------
      # predict upon grid
     
      if ( any( grepl ("predictions", p$spacetime.outputs))) {
        params.local = list()
        if ( exists( "preds_stack_index")) params.local$preds_stack_index = preds_stack_index 
        if ( exists( "pa")) params.local$locs_new = pa[,c("plon", "plat" )]
        preds = spacetime.predict.inla.spde( MESH, RES, p=p, pl=params.local  )
        pa = cbind( pa, preds) 
        rm( params.local, preds, preds_stack_index); gc()
        if ( debugrun) {
          cat( paste(  Sys.time(), Sys.info()["nodename"], "index=", dd,  "predictions completed \n" ), 
              file=p$debug.file, append=TRUE ) 
        }
        if (0) {
          levelplot( xmean ~ plon+plat, pa, aspect="iso", 
                    labels=TRUE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=TRUE) )
          levelplot( xsd   ~ plon+plat, pa, aspect="iso", 
                    labels=TRUE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
        }
        good = which( is.finite( rowSums(pa) ) )
        if (length(good) < 1) next()
        pa = pa[good,]
   
        # update P (predictions)
        counts = 1 # column indices
        means = 2
        stdevs = 3
        ii = pa$i
        test = rowSums( P[ii,] )
        u = which( is.finite( test ) )  # these have data already .. update
        if ( length( u ) > 0 ) {
          ui = ii[u]  # locations of P to modify
          # update counts
          P[ ui, counts ] = P[ ui, counts ] + 1 
          # update SD estimates of predictions with those from other locations via the
          # incremental  method ("online algorithm") of mean estimation after Knuth ; 
          # see https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
          stdev_update =  P[ ui, stdevs ] + ( pa$xsd[u] -  P[ ui, stdevs ] ) / P[ui, counts]
          # update means: inverse-variance weighting   https://en.wikipedia.org/wiki/Inverse-variance_weighting
          means_update = ( P[ ui, means ] / P[ ui, stdevs ]^2 + pa$xmean[u] / pa$xsd[u]^2 ) / ( P[ ui, stdevs]^(-2) + pa$xsd[u]^(-2) )
          mm = which(is.finite( means_update + stdev_update ))
          if( length(mm)> 0) {
            # actual updates occur after everything has been computed first
            P[ ui[mm], stdevs ] = stdev_update[mm]  
            P[ ui[mm], means ]  = means_update[mm]
          }
        }

        # do this as a second pass in case NA's were introduced by the update .. unlikely , but just in case 
        test = rowSums( P[ii,] )
        f = which( !is.finite( test ) ) # first time
        if ( length( f ) > 0 ) {
          fi = ii[f] 
          P[ fi, counts ] = 1
          P[ fi, means ] = pa$xmean[f]
          P[ fi, stdevs ] = pa$xsd[f]
        }
      }
      
      rm(MESH); gc()

      if ( any( grepl ("statistics", p$spacetime.outputs))) {
        # extract summary statistics from a spatial (SPDE) analysis and update the output file
        inla.summary = spacetime.summary.inla.spde2 ( RES, SPDE )
        # save statistics last as this is an indicator of completion of all tasks .. restarts would be broken otherwise
        # S[,(1,2)] are plon, plat
        S[dd,3] = inla.summary["range", "mode"]
        S[dd,4] = inla.summary["range", "sd"]
        S[dd,5] = inla.summary["spatial error", "mode"]
        S[dd,6] = inla.summary["observation error", "mode"]
        if ( debugrun)  {
          print( inla.summary )
          cat( paste(  Sys.time(), Sys.info()["nodename"], "index=", dd,  "statistics saved  \n" ), 
              file=p$debug.file, append=TRUE ) 
        }
      }
      
      rm( SPDE, RES) ; gc()

      if(0) {
        pps = expand.grid( plon=p$plons, plat=p$plats)
        # zz = which(pps$plon > -50 & pps$plon < 50 & pps$plats < 50 & pps$plats > -50 ) # & P[,2] > 0   )
        zz = which(pps$plon > min(pa$plon) & pps$plon < max(pa$plon) & pps$plat < max(pa$plat) & pps$plat > min(pa$plat) ) 
        x11()
        levelplot( ( P[zz,2] ) ~ plon + plat, pps[zz,], aspect="iso", 
                  labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
      }

      rm( ii, good, pa, xs, xm, mi, mf, si, sf ); gc()
      
      if ( debugrun) cat( paste(  Sys.time(), Sys.info()["nodename"], "index=", dd,  "end \n" ), file=p$debug.file, append=TRUE ) 
           
    }
    return( "complete" )
  } ## end core interpolation function definition
  

