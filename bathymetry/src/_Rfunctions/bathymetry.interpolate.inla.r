 
  bathymetry.interpolate.inla = function( ip=NULL, p, plotdata=FALSE ) {

    # ip is the first parameter passed in the parallel mode
    if (exists( "init.files", p)) LoadFiles( p$init.files ) 
    if (exists( "libs", p)) RLibrary( p$libs ) 
    if (is.null(ip)) {
      if( exists( "nruns", p ) ) {
        ip = 1:p$nruns  
      }
    }

    # data file definitions
    P = attach.big.matrix(p$descriptorfile.P , path=p$tmp.datadir )  # predictions
    W = attach.big.matrix(p$descriptorfile.W, path=p$tmp.datadir )  # input data
    S = attach.big.matrix(p$descriptorfile.S , path=p$tmp.datadir )  # statistical outputs

    # priors 
    kappa0 = sqrt(8)/p$expected.range
    tau0 = 1/(sqrt(4*pi)*kappa0*p$expected.sigma)

    for ( iip in ip ) {
      dd = p$runs[ iip, "jj" ]

      # dd=498439
      # inla.setOption(inla.call="/home/jae/tmp/inla.Xeon" ) 
      # inla.setOption(inla.call="/usr/lib/R/library/INLA/bin/linux/inla64" ) 
      focal = S[dd,c(1,2)]  # c(plon, plat)
      
      # choose a distance <= p$dist.max where n is within range of reasonable limits to permit a numerical solution  
      # slow ... need to find a faster solution
      ppp = NULL
      ppp = try( point.in.block( focal, W[,c(1,2)], dist.max=p$dist.max, n.min=p$n.min, n.max=p$n.max ) )
      if( is.null(ppp)) next()
      if (class( ppp ) %in% "try-error" ) next()
      dist.cur = ppp$dist.to.nmax
      j = ppp$indices
      rm(ppp)
  
      if ( (dist.cur/p$dist.max) < 0.1 ) next()

      # .. first create projector from mesh to output
      doff = p$inla.mesh.offset * (dist.cur/p$dist.max) # scale to dist.max
      mbuffer = sum( doff )
      ppdist =  dist.cur - mbuffer 
      if (ppdist < min(p$inla.mesh.max.edge)*5) ppdist = min(p$inla.mesh.max.edge)*5

      preds.diffs = seq( from=-ppdist, to=ppdist, by=p$pres )
      preds.diffs = preds.diffs[ which( preds.diffs >= -dist.cur &  preds.diffs <= dist.cur ) ]
   
      npreds = length(preds.diffs )

      pa_plons = focal[1] + preds.diffs
      pa_plats = focal[2] + preds.diffs

      # range checks.. cc=clipped
      pa_plons_cc = pa_plons[ which( pa_plons >= min(p$plons) & pa_plons <= max(p$plons) ) ]
      pa_plats_cc = pa_plats[ which( pa_plats >= min(p$plats) & pa_plats <= max(p$plats) ) ]

      pa = pa_locs = expand.grid( plons=pa_plons_cc, plats=pa_plats_cc ) # coords of prediction area
      attr(pa, "out.attrs") = NULL  # clipped
      
      pa0 = pa_locs_all = expand.grid( plons=pa_plons, plats=pa_plats ) # coords of prediction area
      attr(pa0, "out.attrs") = NULL # full matrix

      pm_row = round(( pa_locs$plons - p$plons[1]) / p$pres ) + 1
      pm_col = round(( pa_locs$plats - p$plats[1]) / p$pres ) + 1  

      pa_locs     = as.matrix(pa_locs    [, c("plons", "plats")])
      pa_locs_all = as.matrix(pa_locs_all[, c("plons", "plats")])

      Pmat = matrix( 1:(p$nplons*p$nplats), ncol=p$nplats, nrow=p$nplons )  # storage of indices  .. keep here to remove from memory when not needed
      pa$i = Pmat[ cbind(pm_row, pm_col) ]

      rm( Pmat, pm_row, pm_col, pa_plons_cc, pa_plats_cc, pa_plons, pa_plats ) ; gc()

      MESH =try(  inla.mesh.2d ( loc=W[ j, c(1,2)], # locations of data points
        offset = doff ,  # how much to extend inside and outside of boundary
        max.edge= p$inla.mesh.max.edge,  # max size of a triange (in, out)
        cutoff= p$inla.mesh.cutoff # min distance allowed between points 
        # min.angle = c(20),   # min angle (in, out)
        #boundary = inla.nonconvex.hull( W[ j, c(1,2)], resolution=100, concave=p$pres*20 ), 
      ), silent=TRUE )

      if ("try-error" %in% class(MESH)) next()  # some meshes seem to go into a loop .. the processes have to be killed manually .. this catches the error and continues
 
      S0 = inla.spde2.matern( MESH,      
        alpha=p$inla.alpha, # alpha is the Bessel smoothness factor .. 1(?) gives exponential correlation function
        B.tau=matrix(c(log(tau0),-1,+1),nrow=1,ncol=3),
        B.kappa=matrix(c(log(kappa0),0,-1),nrow=1,ncol=3),
        theta.prior.mean=c(0,0), # thetas are the internal representations of prior offsets for tau and kappa (i.e.,range and sigma)
        theta.prior.prec=c(0.1, 0.1) # precision of thetas
      ) 
     
      # data stack 
      Aobs = inla.spde.make.A( mesh=MESH, loc=W[ j, c(1,2)])
      DATA = inla.stack( tag="obs",
        data = list( depth=log( W[j,3] + p$Yoffset ) ), 
        A = list( Aobs, 1), # projection matrix A to translate from mesh nodes to data nodes
        effects = list( 
          c( list(intercept=rep(1,MESH$n )), 
             inla.spde.make.index(name='spatial.field', n.spde=S0$n.spde)),
          covar=rep(1, length(j)))
      )
      rm( Aobs ) 

      # prediction stack
      predict.in.one.go = FALSE
      if( predict.in.one.go) {
        Apreds = inla.spde.make.A(MESH, loc=pa_locs )
        PREDS = inla.stack( tag="preds",
          data=list( depth=NA),
          A=list(Apreds),
          effects=list(
            c( list(intercept=rep(1, MESH$n)),
               inla.spde.make.index( name="spatial.field", MESH$n)))
        )
        rm (Apreds) 
        DATA = inla.stack(DATA, PREDS)
        i_data = inla.stack.index( DATA, "preds")$data
      }
     
      # break into required parts to keep memory usage down
      inputstack = inla.stack.data(DATA)
      A = inla.stack.A(DATA)

      rm(DATA); gc() 
      FM = formula( depth ~ -1 + intercept + f( spatial.field, model=S0 ) )
      RES = try( inla( FM, data = inputstack, 
          control.compute=list(dic=TRUE, config=TRUE), # return linear predictors to compute predictions quickly
          control.results=list(return.marginals.random=FALSE, return.marginals.predictor=FALSE ),
          #control.results=list(return.marginals.random=TRUE, return.marginals.predictor=TRUE ),
          control.predictor=list (A=A, compute=TRUE ), # compute=TRUE on each data location 
          control.inla = list(h =1e-2), # h=0.01 is default step length for gradient calc of hyper params 
          verbose=FALSE
      ), silent=TRUE )

      if ("try-error" %in% class(RES)) { 
        RES = try( inla( FM, data = inputstack, 
          control.compute=list(dic=TRUE, config = TRUE), 
          control.results=list(return.marginals.random=FALSE, return.marginals.predictor=FALSE ),
          control.predictor=list (A=A, compute=TRUE ), 
          control.inla = list(h = 1e-3, tolerance=1e-8, restart=3), # restart a few times in case posteriors are poorly defined
          verbose=FALSE
        ), silent=TRUE )
      }

      if ("try-error" %in% class(RES)) next() # give up 
      
      if ( RES$mode$mode.status > 0) {  # make sure Eignevalues of Hessian are appropriate (>0)
        RES = try( inla( FM, data = inputstack, 
          control.compute=list(dic=TRUE, config = TRUE), 
          control.results=list(return.marginals.random=FALSE, return.marginals.predictor=FALSE ),
          control.predictor=list (A=A, compute=TRUE ), 
          control.inla = list( h=1e-4, tolerance=1e-10), # increase in case values are too close to zero 
          control.mode = list( restart=TRUE, result=RES ), # restart from previous estimates
          verbose=FALSE
        ), silent=TRUE )
      }

      if ("try-error" %in% class(RES)) next()
     
      if ( RES$mode$mode.status > 0) {  # make sure Eignevalues of Hessian are appropriate (>0)
        RES = try( inla( FM, data = inputstack, 
          control.compute=list(dic=TRUE, config = TRUE), 
          control.results=list(return.marginals.random=FALSE, return.marginals.predictor=FALSE ),
          control.predictor=list (A=A, compute=TRUE ), 
          control.inla = list( h=1e-6, tolerance=1e-12), # increase in case values are too close to zero 
          control.mode = list( restart=TRUE, result=RES ), # restart from previous estimates
          verbose=FALSE
        ), silent=TRUE )
      }

      # if still hessian probelems accept the solution .. it should be close enough
      if ("try-error" %in% class(RES)) next()
      
      rm( inputstack, A); gc()
      
      # update statistics
      
      inla.summary = spacetime.inla.extract.parameters( RES, S0, vname="spatial.field" )

      rm( S0); gc() 
      
      # S[,(1,2)] are plon, plat
      S[dd,3] = inla.summary["range", "mode"]
      S[dd,4] = inla.summary["range", "sd"]
      S[dd,5] = inla.summary["spatial error", "mode"]
      S[dd,6] = inla.summary["observation error", "mode"]

      ### TODO compute slope and curvature here ...
      ### S[dd,7] = local.slope( )
      ### S[dd,8] = local.curvature( )
      
      # ----------------
      # predict upon grid
      if ( p$predict.in.one.go ) { 
        # precomputed ... very slow and expensive in RAM/CPU
        # just extract from tag indices
        # not really used/useful as it is way too slow 
        pa$xmean = RES$summary.fitted.values[ i_data, "mean"]  
        pa$xsd   = RES$summary.fitted.values[ i_data, "sd"] 
        
        rm(RES); gc() 

      } else { 

        posterior.samples = inla.posterior.sample(n=p$inla.nsamples, RES)
        rm(RES); gc() 
        
        rnm = rownames(posterior.samples[[1]]$latent )
      
        if ( p$predict.type == "latent.spatial.field" ) {
          i_spatial.field = grep("spatial.field", rnm, fixed=TRUE ) 
          posterior.extract = function(s) { exp(s$latent[ i_spatial.field,1] ) - p$Yoffset  } 
        } 
        
        if ( p$predict.type == "response" ) {
          i_intercept = grep("intercept", rnm, fixed=TRUE ) 
          i_spatial.field = grep("spatial.field", rnm, fixed=TRUE ) 
          posterior.extract = function(s) { exp(s$latent[i_intercept,1] + s$latent[ i_spatial.field,1] ) - p$Yoffset  } 
        }
   
        posterior = sapply( posterior.samples, posterior.extract )
        pG = inla.mesh.projector( MESH, locs=pa_locs_all, dims=c(npreds, npreds) )
        pa0$xmean = c( inla.mesh.project( pG, field=apply( posterior, 1, mean, na.rm=TRUE )  ))
        pa0$xsd   = c( inla.mesh.project( pG, field=apply( posterior, 1, sd, na.rm=TRUE )  ))

        #  mesh projector projects upon the full pa0, redo lattice coordinates for pa
        pa = merge(pa0, pa, all.x=TRUE, all.y=FALSE, sort=FALSE)

        rm (pG, posterior, posterior.samples, rnm, i_intercept, i_spatial.field, pa_locs, pa0 ) ; gc()

      } 

      #xslope = slope.estimate( xmean )
      #xcurv  = curvature.estimate( xmean )


      # levelplot( pa$xmean ~ plons+plats, as.data.frame(pa_locs_all), aspect="iso", col.regions=rev(sequential_hcl(50)), scale=list(draw=FALSE) )
      # levelplot( pa$xsd ~plons+plats, as.data.frame( pa_locs_all), aspect="iso" )

      # merge mean, variance estimates of predictions with those from other locations via the
      # incremental method of mean (and variance) estimation after Knuth ; see 
      # https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance (online algorithm)
      
      good = which( is.finite(pa$i) & is.finite(pa$xmean) ) 
      if (length(good) < 1) next()
      pa = pa[good,]
      ii = pa$i

      P[ii,1] = P[ii,1] + 1 # n
      P[ii,2] = P[ii,2] + ( pa$xmean - P[ii,2] )/P[ii,1] # update mean 
      P[ii,3] = P[ii,3] + ( pa$xsd - P[ii,3] ) /P[ii,1] # update sd

      rm( ii, good, pa, pa_locs_all ); gc()
     
      # pps  =  expand.grid( plons=p$plons, plats=p$plats)
      # levelplot( P[,2] ~plons+plats, pps , col.regions=rev(sequential_hcl(50)), scale=list(draw=FALSE) , aspect="iso" )

    }
  } ## end core interpolation function definition
  

