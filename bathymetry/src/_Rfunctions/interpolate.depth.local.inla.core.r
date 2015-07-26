  interpolate.depth.local.inla.core = function( ip=NULL, p, plotdata=FALSE ) {

    # ip is the first parameter passed in the parallel mode
    if (exists( "init.files", p)) LoadFiles( p$init.files ) 
    if (exists( "libs", p)) RLibrary( p$libs ) 
    if (is.null(ip)) {
      if( exists( "nruns", p ) ) {
        ip = 1:p$nruns  
      }
    }

    # data file definitions
    Pmat = attach.big.matrix(p$descriptorfile.Pmat , path=p$tmp.datadir )  # indices of predictions in matrix form
    P = attach.big.matrix(p$descriptorfile.P , path=p$tmp.datadir )  # predictions
    W = attach.big.matrix(p$descriptorfile.W, path=p$tmp.datadir )  # input data
    S = attach.big.matrix(p$descriptorfile.S , path=p$tmp.datadir )  # statistical outputs
   
    # zrange = range( W[,3], na.rm=TRUE )
    
    preds.diffs = seq(from=-p$dist.pred, to=p$dist.pred,  by=p$pres )
    npreds = length(preds.diffs )

    for ( iip in ip ) {
      dd = p$runs[ iip, "jj" ]

      focal = S[dd,c(1,2)]  # c(plon, plat)
      dists = sqrt( (focal[,1] - W[,1] )**2 + (focal[,2] - W[,2] )**2 ) 
      j = which( dists <= p$dist.max )
      if (length(j) < p$n.min ) next()

      indat = data.frame(b0=rep(1, length(j)) )  # intercepts used later
      indat$plon = W[ j, 1]
      indat$plat = W[ j, 2]
      indat$z = W[ j, 3]
      
      distances.median = median( dists[j], na.rm=TRUE )
      rm(dists, j); gc()

      # compute stats
      locs  = as.matrix( indat[,c("plon", "plat")] )
      domain = inla.nonconvex.hull( locs, convex=distances.median )
      MESH = inla.mesh.2d (
        loc=locs , # locations of data points
        boundary = domain, 
        offset = distances.median * c(2, 4 ),  # how much to extend inside and outside of boundary
        max.edge= p$pres* c(1, 5 ),  # max size of a triange (in, out)
        # min.angle = c(20),   # min angle (in, out)
        cutoff= p$pres * c(1, 10)  # min distance allowed  /.... use 8 or less for production 
      )
      rm (domain); gc() 
      
      S0 = inla.spde2.matern( MESH, alpha=p$inla.alpha ) # alpha=2 is exponential correlation function
      i <- inla.spde.make.index('i', n.spde=S0$n.spde )  # indices of SPDE 

      # projection matrix A to translate from mesh nodes to data nodes
      A = inla.spde.make.A( mesh=MESH, loc=locs )

      # data stack 
      DATA = inla.stack( tag="bathymetry", data=list( depth=(indat$z) ), A=list(A,1), effects=list( i=i, indat ))
      rm (indat, A, i ) ; gc()

      RES <- try( inla(
          depth ~ -1 + b0 + f( i, model=S0 ),
          data=inla.stack.data(DATA), 
          control.compute=list(config=FALSE, dic=TRUE), # return linear predictors to compute predictions quickly
          control.results=list(return.marginals.random=FALSE, return.marginals.predictor=FALSE ),
          # control.results=list(return.marginals.random=TRUE, return.marginals.predictor=TRUE ),
          control.predictor=list(A=inla.stack.A(DATA), compute=FALSE ), # compute=TRUE on each data location 
          control.inla = list(h = 0.02) # h=0.01 is step length for gradient calc of hyper params 
      ), silent=TRUE )

      if ("try-error" %in% class(RES)) next()

      # update statistics
      inla.summary = spacetime.inla.extract.parameters( RES, S0, vname="i" )
      rm(S0); gc() 

      # S[,(1,2)] are plon, plat
      S[dd,3] = inla.summary["range", "mode"]
      S[dd,4] = inla.summary["range", "sd"]
      S[dd,5] = inla.summary["spatial error", "mode"]
      S[dd,6] = inla.summary["observation error", "mode"]

      # ----------------
      # predict upon grid
      # prediction (of the latent field) for each time and visualize it 
      
      # .. first create projector from mesh to output
      pa = expand.grid( plons=focal[,1] + preds.diffs, plats=focal[,2] + preds.diffs ) # coords of prediction area
      pm_row = round(( pa$plons - p$plons[1]) / p$pres ) + 1
      pm_col = round(( pa$plats - p$plats[1]) / p$pres ) + 1  

      inarea = which( rowcol[,1] > 0 & rowcol[,1] <= p$nplons &
                      rowcol[,2] > 0 & rowcol[,2] <= p$nplats )
      if (length(inarea) < 3 ) next()
      pa = pa[inarea,]
      pa$index = Pmat[ pm_row[inarea], pm_col[inarea] ]

      pG = inla.mesh.projector( MESH, xlim=range(pa$plons), ylim=range(pa$plats), dims=c( diff(range(pa$plons)), diff(range(pa$plons)) )  )
      # inside = inout( pG$lattice$loc, domain$loc ) 
      
      xmean = inla.mesh.project( pG, RES$summary.random$i$mean )
      xsd   = inla.mesh.project( pG, RES$summary.random$i$sd )
      
      rm( pm_row, pm_col, RES, pG) ; gc()

      xmean = as.vector(xmean) 
      xsd   = as.vector(xsd)
      
      # ee = which( xmean < zrange[1] )
      # ff = which( xmean > zrange[2] )
      # if (length(ee) > 0 ) xmean[ee ] = zrange[1]  # do not permit extrapolation
      # if (length(ff) > 0 ) xmean[ff ] = zrange[2]  # do not permit extrapolation

      # merge mean, variance estimates of predictions with those from other locations via the
      # incremental method of mean (and variance) estimation 
      # after Knuth ; see 
      # https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance (online algorithm)
      
      good = which(is.finite(xmean))
      if (length(good) < 1) next()
      uu = pa$index[good] 
      rm(pa); gc()

      P[uu,1] = P[uu,1] + 1 # n
      P[uu,2] = P[uu,2] + ( xmean[good] - P[uu,2] )/P[uu,1] # update mean 
      P[uu,3] = P[uu,3] + ( xsd[good] - P[uu,3] ) /P[uu,1] # update sd

      rm(uu, good, xmean, xsd); gc()
      
      plotdata=FALSE
      if (plotdata) { 
        print (dd)
        oo = P[] # make a copy as we will introduce NA's
        nd = which( oo[,1]==0 )
        if (length(nd)>0) oo[ nd,2 ] = NA # no data .. no mean
        means.grid = matrix( data=oo[,2], nrow=length(p$plons), ncol=length(p$plats) )
        lv = levelplot( ( means.grid) , xlab='', ylab='', col.regions=topo.colors(200), scale=list(draw=FALSE), aspect="iso" )
        print(lv)
      }
    }
  } ## end core interpolation function definition
  

