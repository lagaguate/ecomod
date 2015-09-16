 
  spacetime.interpolate.inla = function( ip=NULL, p, debugrun=FALSE ) {

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
      focal = t(S[dd,])
      if (focal[3] != 0) next()
      S[dd,3] = p$fail.flag   # this is a flag such that if a run fails (e.g. in mesh generation), it does not get revisited
      # .. it gets over-written below if successful

      # choose a distance <= p$dist.max where n is within range of reasonable limits to permit a numerical solution  
      # slow ... need to find a faster solution
      ppp = NULL
      ppp = try( point.in.block( focal[1,c(1,2)], W[,c(1,2)], dist.max=p$dist.max, n.min=p$n.min, n.max=p$n.max, shrink=TRUE ) )
      if( is.null(ppp)) next()
      if (class( ppp ) %in% "try-error" ) next()
      dist.cur = ppp$dist.to.nmax
      j = ppp$indices
      rm(ppp)
 
      if ( debugrun)  cat( paste(  Sys.time(), Sys.info()["nodename"], "index=", dd, "n=", length(j), "dist=", dist.cur, "\n" ), file=p$debug.file, append=TRUE ) 
      
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
      pa0 = expand.grid( plons=pa_plons, plats=pa_plats ) # coords of full prediction area
      attr( pa0, "out.attrs") = NULL

      # range checks.. cc=clipped
      pa_plons_cc = pa_plons[ which( pa_plons >= min(p$plons) & pa_plons <= max(p$plons) ) ]
      pa_plats_cc = pa_plats[ which( pa_plats >= min(p$plats) & pa_plats <= max(p$plats) ) ]
      pa = expand.grid( plons=pa_plons_cc, plats=pa_plats_cc ) # coords of clipped prediction area
      attr( pa, "out.attrs") = NULL
      pm_row = round(( pa$plons - p$plons[1]) / p$pres ) + 1
      pm_col = round(( pa$plats - p$plats[1]) / p$pres ) + 1  

      Pmat = matrix( 1:(p$nplons*p$nplats), ncol=p$nplats, nrow=p$nplons )  # storage of indices  .. keep here to remove from memory when not needed
      pa$i = Pmat[ cbind(pm_row, pm_col) ]

      rm( Pmat, pm_row, pm_col, pa_plons_cc, pa_plats_cc, pa_plons, pa_plats ) ; gc()

      locs = W[ j, c(1,2)] + runif( length(j)*2, min=-p$pres*0.025, max=p$pres*0.025 ) 

      ydata = log( W[j,3] + p$Yoffset )
      rm(j); gc()

      lengthscale=dist.cur*2 
   
      # note using convex hull boundaries below makes the solutions hang for unkown reasons ... do not use(yet)
      # also sending direct distances rather than proportion seems to cause issues..
      M = NULL
      M =try(  
        inla.mesh.2d ( 
            loc=locs, # locations of data points
            max.edge = p$inla.mesh.max.edge * lengthscale,  # max size of a triange (in, out) proportion of dist.max
            offset = -p$inla.mesh.offset ,  # how much to extend inside and outside of boundary: proportion of dist.max
            cutoff = -p$inla.mesh.cutoff, # min distance allowed between points: proportion of dist.max 
            #boundary = list( 
            #  inla.nonconvex.hull(locs, convex=p$inla.mesh.hull.radius[1]*lengthscale, resolution=p$inla.mesh.hull.resolution ) ,  
            #  inla.nonconvex.hull(locs, convex=p$inla.mesh.hull.radius[2]*lengthscale, resolution=p$inla.mesh.hull.resolution ) ) 
        ), silent=TRUE 
      )

      if ( "try-error" %in% class(M)) next()
      if ( is.null(M) ) next()  # some meshes seem to go into an infinite loop 
 
      S0 = inla.spde2.matern( M,      
        alpha=p$inla.alpha, # alpha is the Bessel smoothness factor .. 1(?) gives exponential correlation function
        B.tau=matrix(c(log(tau0),-1,+1),nrow=1,ncol=3),
        B.kappa=matrix(c(log(kappa0),0,-1),nrow=1,ncol=3),
        theta.prior.mean=c(0,0), # thetas are the internal representations of prior offsets for tau and kappa (i.e.,range and sigma)
        theta.prior.prec=c(0.1, 0.1) # precision of thetas
      ) 
     
      # data stack 
      Aobs = inla.spde.make.A( mesh=M, loc=locs )
      DATA = inla.stack( tag="obs",
        data = list( ydata=ydata ), 
        A = list( Aobs, 1), # projection matrix A to translate from mesh nodes to data nodes
        effects = list( 
          c( list(intercept=rep(1,M$n )), 
             inla.spde.make.index(name='spatial.field', n.spde=S0$n.spde)),
          covar=rep(1, length(ydata)))
      )
      rm( Aobs ) 

      # prediction stack
      predict.in.one.go = FALSE
      if( predict.in.one.go) {
        Apreds = inla.spde.make.A(M, loc=as.matrix(pa[, c("plons", "plats")]) )
        PREDS = inla.stack( tag="preds",
          data=list( ydata=NA),
          A=list(Apreds),
          effects=list(
            c( list(intercept=rep(1, M$n)),
               inla.spde.make.index( name="spatial.field", M$n)))
        )
        rm (Apreds) 
        DATA = inla.stack(DATA, PREDS)
        i_data = inla.stack.index( DATA, "preds")$data
      }
     
      RES = NULL
      RES = spacetime.inla.call( FM=p$modelformula, inputstack=inla.stack.data(DATA), A=inla.stack.A(DATA) )
      if (is.null(RES)) next()
      
      # update statistics
      inla.summary = spacetime.inla.extract.parameters( RES, S0, vname="spatial.field" )
      rm( S0); gc() 
      
      # S[,(1,2)] are plon, plat
      S[dd,3] = inla.summary["range", "mode"]
      S[dd,4] = inla.summary["range", "sd"]
      S[dd,5] = inla.summary["spatial error", "mode"]
      S[dd,6] = inla.summary["observation error", "mode"]

      # ----------------
      # predict upon grid

      if ( exists("predict.in.one.go", p)) {
        if ( p$predict.in.one.go ) { 
      
        # precomputed ... very slow and expensive in RAM/CPU ..not really used/useful as it is way too slow 
        # just extract from tag indices
        pa$xmean = RES$summary.fitted.values[ i_data, "mean"]  
        pa$xsd   = RES$summary.fitted.values[ i_data, "sd"] 
        rm(RES); gc() 
        }
      
      } else { 
        
        # project from MESH upon the full pa0
        pG = inla.mesh.projector( M, locs=as.matrix( pa0 ), dims=c(npreds, npreds) )
        posterior.samples = inla.posterior.sample(n=p$inla.nsamples, RES)
        rm(RES, M); gc()

        rnm = rownames(posterior.samples[[1]]$latent )
        posterior = sapply( posterior.samples, p$spacetime.posterior.extract, rnm=rnm )
        rm(posterior.samples); gc()

        pa0$xmean = c( inla.mesh.project( pG, field=apply( posterior, 1, mean, na.rm=TRUE )  ))
        pa0$xsd   = c( inla.mesh.project( pG, field=apply( posterior, 1, sd, na.rm=TRUE )  ))
        rm( posterior ); gc() 
        
        # merge in lattice coordinates for pa (and remove missing locations .. below)
        pa = merge(pa0, pa, by=c("plons", "plats"), all.x=TRUE, all.y=FALSE, sort=FALSE)
        rm(pa0); gc()
      } 
   
      if (0) levelplot( xmean ~ plons+plats, pa, aspect="iso" )

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

      rm( ii, good, pa ); gc()
     
    }
  } ## end core interpolation function definition
  

