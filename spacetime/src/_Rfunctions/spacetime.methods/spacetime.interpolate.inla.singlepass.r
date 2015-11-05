 
  spacetime.interpolate.inla.singlepass = function( dat, locs, locsout, lengthscale=NULL, method="fast", link="identity", nsamples=5000 ) {
    #\\ low-level function -- single pass/fast spatial interpolator using inla 
    #\\   but no bigmemory objects and no stats, etc. .. prediction only, serial mode only
    #\\   lengthscale is the range (prior) 
    #\\   method="fast" using an indirect estimate based upon posterior projections of the input
    #\\   method="direct" uses direct (more accurate) estimation but way too slow and resource 
    #\\     intensive to be useful for production runs .. used only to check the fast method 
 
    # identity links by default .. add more if needed here
    spacetime.link = function(X) {X}
    spacetime.invlink = function(X) {X}

    if (link=="log" ) {
      spacetime.link = function(X) {log(X)}
      spacetime.invlink = function(X) {exp(X)}
    } 
    
    locs = as.matrix( locs)

    if (is.null(lengthscale)) lengthscale = max( diff(range( locs[,1])), diff(range( locs[,2]) )) / 10  # in absence of range estimate take 1/10 of domain size
    sigmahat = sd( spacetime.link(dat) )

    # priors in internal/inla scale 
    kappa0 = sqrt(8)/ lengthscale
    tau0 = 1/(sqrt(4*pi)*kappa0*sigmahat )
  
    ndata = length(dat)
    noise = lengthscale * 1e-9 
    locs = locs + runif( ndata*2, min=-noise, max=noise ) # add  noise  to prevent a race condition .. inla does not like uniform grids

    MESH = spacetime.mesh( locs, lengthscale=lengthscale ) 
   
    if ( is.null( MESH) ) return( "Mesh Error" )

    SPDE = inla.spde2.matern( MESH,      
      alpha=2 , # alpha is the Bessel smoothness factor .. 1(?) gives exponential correlation function
      B.tau=matrix(c(log(tau0),-1,+1),nrow=1,ncol=3),
      B.kappa=matrix(c(log(kappa0),0,-1),nrow=1,ncol=3),
      theta.prior.mean=c(0, 0), # thetas are the internal representations of prior offsets for tau and kappa (i.e.,range and sigma)
      theta.prior.prec=c(0.1, 0.1) # precision of thetas
    ) 
 
    # data stack 
    DATA = inla.stack( tag="obs",
      data = list( ydata= spacetime.link( dat ) ), 
      A = list( inla.spde.make.A( mesh=MESH, loc=locs ), 1), # projection matrix A to translate from mesh nodes to data nodes
      effects = list( 
        c( list(intercept=rep(1,MESH$n )), 
           inla.spde.make.index(name="spatial.field", n.spde=SPDE$n.spde)),
        covar=rep(1, ndata ))
    )
     
    if ( method=="direct") {
      # direct method
        Apreds = inla.spde.make.A(MESH, loc=as.matrix( locsout ) )
        PREDS = inla.stack( tag="preds",
          data=list( ydata=NA),
          A=list(Apreds),
          effects=list(
            c( list(intercept=rep(1, MESH$n)),
               inla.spde.make.index( name="spatial.field", MESH$n)))
        )
        DATA = inla.stack(DATA, PREDS)
        
        i_data = inla.stack.index( DATA, "preds")$data
    }

    FM = formula( ydata ~ -1 + intercept + f( spatial.field, model=SPDE ) ) 
      
    RES = NULL
    RES = spacetime.inla.call( FM=FM, DATA=DATA, SPDE=SPDE, FAMILY="gaussian" )
     
    # inla.spde2.matern creates files to disk that are not cleaned up:
    spdetmpfn = SPDE$f$spde2.prefix
    fns = list.files( dirname( spdetmpfn ), all.files=TRUE, full.names=TRUE, recursive=TRUE, include.dirs=TRUE )  
    oo = grep( basename(spdetmpfn), fns )
    if(length(oo)>0) file.remove( sort(fns[oo], decreasing=TRUE) )
   
    rm( SPDE, DATA ); gc() 
 
    # predict upon grid

    if ( method=="direct" ) {
      # direct method ... way too slow to use for production runs
      preds = as.data.frame( locsout )
      preds$xmean = spacetime.invlink( RES$summary.fitted.values[ i_data, "mean"] )
      preds$xsd   = spacetime.invlink( RES$summary.fitted.values[ i_data, "sd"] )
      rm(RES, MESH); gc()
    }   

    if (method=="fast") {
      posterior.extract = function(s, rnm) { 
        # rnm are the rownames that will contain info about the indices ..
        # optimally the grep search should only be done once but doing so would 
        # make it difficult to implement in a simple structure/manner ... 
        # the overhead is minimal relative to the speed of modelling and posterior sampling
        i_intercept = grep("intercept", rnm, fixed=TRUE ) # matching the model index "intercept" above .. etc
        i_spatial.field = grep("spatial.field", rnm, fixed=TRUE )
        return(  s$latent[i_intercept,1] + s$latent[ i_spatial.field,1] )
      }

      # note: locsout seems to be treated as token locations and really its range and dims controls output 
      pG = inla.mesh.projector( MESH, loc=as.matrix( locsout ) )
      posterior.samples = inla.posterior.sample(n=nsamples, RES)
      rm(RES, MESH); gc()
      
      rnm = rownames(posterior.samples[[1]]$latent )  
      posterior = sapply( posterior.samples, posterior.extract, rnm=rnm )
      posterior = spacetime.invlink( posterior )   # return to original scale
      rm(posterior.samples); gc()
  
      if ( exists( "predict.quantiles", p ) ) {
          # robustify the predictions by trimming extreme values .. will have minimal effect upon mean
          # but variance estimates should be useful/more stable as the tails are sometimes quite long 
          for (ii in 1:nrow(posterior )) {
            qnt = quantile( posterior[ii,], probs=p$predict.quantiles, na.rm=TRUE ) 
            toolow = which( posterior[ii,] < qnt[1] )
            toohigh = which (posterior[ii,] > qnt[2] )
            if (length( toolow) > 0 ) posterior[ii,toolow] = qnt[1]
            if (length( toohigh) > 0 ) posterior[ii,toohigh] = qnt[2]
          }
      }

      # posterior projection is imperfect right now .. not matching the actual requested locations
      preds = data.frame( plon=pG$loc[,1], plat = pG$loc[,2])
      preds$xmean = c( inla.mesh.project( pG, field=apply( posterior, 1, mean, na.rm=TRUE )  ))
      preds$xsd   = c( inla.mesh.project( pG, field=apply( posterior, 1, sd, na.rm=TRUE )  ))
      rm (pG)
    }

    if (0) {
      levelplot( log( xmean)  ~ plon+plat, preds, aspect="iso", 
                labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
      levelplot( log (xsd )  ~ plon+plat, preds, aspect="iso", 
                labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
    }

    return( preds )
  } 
  

