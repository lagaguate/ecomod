 
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
      focal = S[dd,c(1,2)]  # c(plon, plat)
      
      # choose a distance <= p$dist.max where n is within range of reasonable limits to permit a numerical solution  
      # slow ... need to find a faster solution
      dist.cur = NULL
      dist.cur = try( point.in.block( focal, W[,c(1,2)], dist.max=p$dist.max, n.max=p$n.max, returnvalue="dist.to.nmax" ) )
      if (class(dist.cur) %in% "try-error" ) dist.cur = p$dist.pred

      # .. first create projector from mesh to output
      preds.diffs = seq( from=-p$dist.pred, to=p$dist.pred, by=p$pres )
      preds.diffs = preds.diffs[ which( preds.diffs >= -dist.cur &  preds.diffs <= dist.cur ) ]
   
      npreds = length(preds.diffs )

      pa_plons = focal[1] + preds.diffs
      pa_plats = focal[2] + preds.diffs

      # range checks
      pa_plons = pa_plons[ which( pa_plons >= min(p$plons) & pa_plons <= max(p$plons) ) ]
      pa_plats = pa_plats[ which( pa_plats >= min(p$plats) & pa_plats <= max(p$plats) ) ]

      pa_locs = expand.grid( plons=pa_plons, plats=pa_plats ) # coords of prediction area
      pm_row = round(( pa_locs$plons - p$plons[1]) / p$pres ) + 1
      pm_col = round(( pa_locs$plats - p$plats[1]) / p$pres ) + 1  

      Pmat = matrix( 1:(p$nplons*p$nplats), ncol=p$nplats, nrow=p$nplons )  # storage of indices  .. keep here to remove from memory when not needed
      pa_index = Pmat[ cbind(pm_row, pm_col) ]
      
      pa_locs = as.matrix(pa_locs[, c("plons", "plats")])

      rm( Pmat, pm_row, pm_col, pa_plons, pa_plats, preds.diffs ) ; gc()

      MESH = inla.mesh.2d ( loc=W[ j, c(1,2)], # locations of data points
        offset = p$inla.mesh.offset ,  # how much to extend inside and outside of boundary
        max.edge= p$inla.mesh.max.edge,  # max size of a triange (in, out)
        cutoff= p$inla.mesh.cutoff # min distance allowed  /.... use 8 or less for production 
        # min.angle = c(20),   # min angle (in, out)
        #boundary = inla.nonconvex.hull( W[ j, c(1,2)], resolution=100, concave=p$pres*20 ), 
      )
      
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
          A=Apreds,
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

      rm(DATA, locs); gc() 

      RES = try( inla(
          depth ~ -1 + intercept + f( spatial.field, model=S0 ),
          data = inputstack, 
          control.compute=list(dic=TRUE, config = TRUE), # return linear predictors to compute predictions quickly
          control.results=list(return.marginals.random=FALSE, return.marginals.predictor=FALSE ),
          #control.results=list(return.marginals.random=TRUE, return.marginals.predictor=TRUE ),
          control.predictor=list (A=A, compute=TRUE ), # compute=TRUE on each data location 
          control.inla = list(h = 0.05), # h=0.01 is step length for gradient calc of hyper params 
          num.threads=2,
          working.directory=file.path( tempdir(), Sys.info()["nodename"], dd ), 
          # verbose=TRUE
          verbose=FALSE
      ), silent=TRUE )

      rm( inputstack, A); gc()

      if ("try-error" %in% class(RES)) next()

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
        xmean = RES$summary.fitted.values[ i_data, "mean"]  
        xsd   = RES$summary.fitted.values[ i_data, "sd"] 
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
        pG = inla.mesh.projector( MESH, locs=pa_locs, dims=c(npreds, npreds) )
        xmean = c( inla.mesh.project( pG, field=apply( posterior, 1, mean )  ))
        xsd   = c( inla.mesh.project( pG, field=apply( posterior, 1, sd )  ))
        

        rm (pG, posterior.samples, rnm, i_intercept, i_spatial.field, pa_locs ) ; gc()

      } 

      #xslope = slope.estimate( xmean )
      #xcurv  = curvature.estimate( xmean )


      # levelplot( xmean ~ plons+plats, as.data.frame.table(pa_locs), aspect="iso", col.regions=rev(sequential_hcl(50)), scale=list(draw=FALSE) )
      # levelplot( xsd ~plons+plats, pa_locs, aspect="iso" )

      # merge mean, variance estimates of predictions with those from other locations via the
      # incremental method of mean (and variance) estimation after Knuth ; see 
      # https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance (online algorithm)
      
      good = which(is.finite(xmean))
      if (length(good) < 1) next()
      uu = pa_index[good] 
      rm(pa_index); gc()

      P[uu,1] = P[uu,1] + 1 # n
      P[uu,2] = P[uu,2] + ( xmean[good] - P[uu,2] )/P[uu,1] # update mean 
      P[uu,3] = P[uu,3] + ( xsd[good] - P[uu,3] ) /P[uu,1] # update sd

      rm(uu, good, xmean, xsd); gc()
     
      # pps  =  expand.grid( plons=p$plons, plats=p$plats)
      # levelplot( P[,2] ~plons+plats, pps , col.regions=rev(sequential_hcl(50)), scale=list(draw=FALSE) , aspect="iso" )

    }
  } ## end core interpolation function definition
  

