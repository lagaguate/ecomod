
variogram.ecomod( xyz, crs="+proj=utm +zone=20 +ellps=WGS84" ) {
  
  # estimate empirical variograms and then model them using a number of different approaches
  # returns empirical variogram and parameter estimates, and optionally the models themselves
  # expect xyz = c(lon, lat, variable)

  require(sp)
  require(gstat)
  require(INLA)

  if ( xyz=="test") {
    # just for debugging / testing ...
    require(sp)
    data(meuse)
    xyz = meuse[, c("x", "y", "elev")]
  }

  if ( !grepl( "planar", crs )) { 
    # i.e. if not already planar coords, then  assume it is in lon-lat .. requires some planar coord system
    nm = names(xyz) 
    xyz = try( lonlat2planar( xyz, proj.type=crs ), silent=TRUE )
    xyz = xyz[, c("plon", "plat", nm[3])]
  } 
    
  names(xyz) =  c("plon", "plat", "z" )
 
  # first pass -- use gstat to obtain fast estimates of variogram parameters to speed up inla
    vEm = variogram( z~1, locations=~plon+plat, data=xyz ) # empirical variogram
    vMod0 = vgm(psill=0.5, model="Mat", range=mean(drange), nugget=0.5, kappa=2 ) # starting model parameters
    vFitgs =  fit.variogram( vEm, vMod0 ) ## gstat's kappa is the Bessel function's "nu" smoothness parameter
      
    vRange = vFitgs$range[2] # 95% of total variance 
    vTot   = vFitgs$psill[1] + vFitgs$psill[2] 
    vPsill = vFitgs$psill[2]  
    vNugget = vFitgs$psill[1]   
    vMod1 =  vgm(psill=vPsill, model="Mat", range=vRange, nugget=vNugget, kappa=2 )

    plot( vEm, model=vFitgs )

  # now inla
    inla.setOption(scale.model.default = TRUE)  # better numerical performance of IGMRF models and less dependnence upon hyperpriors

    locs0  = as.matrix( xyz[,1:2] )
    M0.domain = inla.nonconvex.hull( locs0 )
    M0 = inla.mesh.2d (
      loc=locs0, # locations of data points
      boundary=M0.domain,
      max.edge=vRange
    )
    
    plot(M0, asp=1 ) # visualise mesh

    S0 = inla.spde2.matern( M0, alpha=2 ) # alpha=2 is exponential correlation function
    i <- inla.spde.make.index('i', n.spde=S0$n.spde )  

    # projection matrix A to translate from mesh nodes to data nodes
    A = inla.spde.make.A( mesh=M0, loc=locs0 )
    z = xyz$z
    xyz$z = NULL
    xyz$b0 = 1

    # data stack for occurence (PA)
    Z = inla.stack( 
        tag="data",
        data=list( z=z ) ,
        A=list(A,1),
        effects=list( i=i, xyz ) 
    )
      
    R <- inla(  z ~ 0 + b0+ f( i, model=S0), 
        data=inla.stack.data(Z), 
        control.compute=list(dic=TRUE),
#        control.results=list(return.marginals.random=TRUE, return.marginals.predictor=TRUE ),
        control.predictor=list(A=inla.stack.A(Z), compute=TRUE),
#        control.inla=list(h=0.05, strategy="laplace", npoints=21, stencil=7 , strategy='gaussian' ),
        verbose=FALSE
    )


    graphics.off()
    # field parameters on user scale
    oo = inla.spde2.result(R, 'i', S0, do.transf=TRUE)
    exp(oo$summary.log.range.nominal)
    exp(oo$summary.log.variance.nominal)
    exp(oo$summary.log.kappa)
    exp(oo$summary.log.tau)


    plot(oo$marginals.variance.nominal[[1]], type='l', xlab=expression(sigma[x]), ylab='Density')
    # abline(v=params[1], col=2)

    plot(oo$marginals.kappa[[1]], type='l', xlab=expression(kappa), ylab='Density')
    # abline(v=params[2], col=2)

    plot(oo$marginals.range.nominal[[1]], type='l', xlab='range nominal', ylab='Density')
    # abline(v=sqrt(8)/params[2], col=2)
   
    
    # indices for random field at data locations
    idat <- inla.stack.index( Z, 'data')$data

    # correlation between the the posterior mean and the response by
    cor( z, R$summary.linear.predictor$mean[idat])



    plot( R$marginals.fixed[[1]], type="l", xlab="Beta" )



    plot( R$marginals.fixed[["b0_PA"]], type="l", xlab="b0_PA" )
    plot( R$summary.random[["i"]][,c("ID", "mean")], type="l", xlab="" )


    str(R$marginals.hyperpar)
    plot( R$marginals.hyper[[1]], type="l", xlab="" )
    plot.default( inla.tmarginal( function(x) {1/exp(x)}, R$marginals.hyperpar[[1]]), xlab="", type="l")




~
~

# equivalent Random Field representation using geostatsinla
     
      require( geostatsinla)

      fm =  formula( z ~ 1 ) 
      # rw2 is an ~ GAM
      # inla.group discretizes the data into smaller number of unique values for 
      # numerical efficiency and avoiding singular solutions
      
      ft = glgm( data=xyz, formula=fm, family="normal",
        cells=100, shape=2, buffer=100, 
        priorCI=list(sd = c(0.1, 10), range=c(10, 100)), 
        # control.compute=list(dic=TRUE, cpo=TRUE),
        # control.predictor=list(compute=T), # compute marginal distributions for each value of the linear predictor
        # control.inla=list(int.strategy = "grid", diff.logdens = 4, strategy = "laplace", npoints = 21), # better handle on tails
        #debug=TRUE, verbose=TRUE,    
        control.compute=list(dic=TRUE)
      )

      names(ft$parameter)
      plot(ft$parameter[["range"]]$posterior, type="l", col="green" )
      lines(ft$parameter[["range"]]$prior )

      plot(ft$parameter[["sd"]]$posterior, type="l", col="green" )
      lines(ft$parameter[["sd"]]$prior )

      names(ft$raster)
      plot(ft$raster[["predict.mean"]])
      plot(ft$raster[["random.mean"]])


      (ft$parameters$summary)
 
      R = ft$inla
      (R$summary.fixed)  # intercept
      

      post.se = inla.tmarginal( function(x){ sqrt(1/x) }, R$marginals.hyperpar[[1]] ) # transforms precision to se scale
      inla.zmarginal( post.se ) # general stats on the marginal of hyper params  ... ie, Nugget .. 


      post.range.se = inla.tmarginal( function(x){ sqrt(1/x) }, R$marginals.hyperpar[["Range for space"]] ) 
      inla.zmarginal( post.range.se )

      fp = data.frame( rasterToPoints( ft$raster[["predict.mean"]] ) )
      names(fp) = c("plon", "plat", "S")

      region = "cfanorth"
      region = "cfasouth"

      i = filter.region.polygon(x=fp[,c("plon", "plat")], region=region, planar=T)
      fp = fp[i,]
      require(lattice)
      levelplot( B~plon+plat, data=fp, aspect="iso") 
      exc30 = excProb( ft, 1, nuggetInPrediction = TRUE) # conditional probabilities that  y >1     
        # nuggetInPrediction argument can be set to TRUE to compute probabilities of new
        # observations Y i exceeding a threashold, with FALSE specifying exceedance probabilities for
        # λ(s)



      # Prior 95% intervals for σ and φ are specified, 
      # glgm creates Gamma priors for the precision 1/σ^2 and a scaled range parameter φ/δ (with δ being the cell size) having the 95% intervals specified
      # inla requires priors to be continuous, but are otherwise unrestricted
      # also, priors are set for log precisions, with prior distributions available including the log-Gamma and Normal
      # Priors for the remaining parameters can be specified with inla arguments such as 
      #   control.fixed=list(prec.intercept=0.01) 


      # glgm returns: 
      #   inla (raw inla results)
      #   parameters - prior and posteriors; and "parameters$summary"
      #   raster (stack) - posterior means of random effects and fitted values on link scale g[lambda(s)] == raster[["random.mean"]]
      #     -- raster[["predict.invlogit"]] == posterior means of lamda(s)
     


  }




  if ("gstat" %in% method) {
    require(gstat)
  
  
  }


}


