
variogram.ecomod = function( xyz, crs="+proj=utm +zone=20 +ellps=WGS84", plot=FALSE, edge=c(1/3, 1), return.inla=FALSE ) {
  
  # estimate empirical variograms and then model them using a number of different approaches
  # returns empirical variogram and parameter estimates, and optionally the models themselves
  # expect xyz = c(lon, lat, variable)

  require(sp)
  require(gstat)
  require(INLA)
  require(lattice)
  
  out = NULL

 if ( "test" %in% xyz ) {
    # just for debugging / testing ...
   loadfunctions("utility")
   loadfunctions("spacetime")
   data(meuse)
    xyz = meuse[, c("x", "y", "elev")]
    crs="+proj=utm +zone=20 +ellps=WGS84"
    edge=c(1/3, 1)
  }

   
  if ( !grepl( "planar", crs )) { 
    # i.e. if not already planar coords, then  assume it is in lon-lat .. requires some planar coord system
    nm = names(xyz) 
    xyz = try( lonlat2planar( xyz, proj.type=crs ), silent=TRUE )
    xyz = xyz[, c("plon", "plat", nm[3])]
  } 
  
  names(xyz) =  c("plon", "plat", "z" )
  
  drange = sqrt(diff(range(xyz$plon))^2 + diff(range(xyz$plat))^2)
  
  xrange = range( xyz$plon, na.rm=TRUE )
  yrange = range( xyz$plat, na.rm=TRUE )
  zrange = range( xyz$z, na.rm=TRUE )
  
  difx = diff( xrange) 
  dify = diff( yrange) 

  nn = 400
  nxout = trunc(nn * difx / dify)
  nyout = nn
  nzout = 100

  xx = seq( xrange[1], xrange[2], length.out=nxout )
  yy = seq( yrange[1], yrange[2], length.out=nyout )
  zz = seq( zrange[1], zrange[2], length.out=nzout )
  preds = expand.grid( plon=xx, plat=yy )

  # first pass -- use gstat to obtain faster estimates of variogram parameters to speed up inla
  vEm = variogram( z~1, locations=~plon+plat, data=xyz ) # empirical variogram
  vMod0 = vgm(psill=0.5, model="Mat", range=drange/10, nugget=0.5, kappa=2 ) # starting model parameters
  vFitgs =  fit.variogram( vEm, vMod0 ) ## gstat's kappa is the Bessel function's "nu" smoothness parameter
    
  vRange = vFitgs$range[2] # 95% of total variance 
  vTot   = vFitgs$psill[1] + vFitgs$psill[2] 
  vPsill = vFitgs$psill[2]  
  vNugget = vFitgs$psill[1]   


  if (plot) {
    x11()
    plot( vEm, model=vFitgs, main="gstat Variogram" )
    g <- gstat(id = "elev", formula = z~1, locations = ~plon+plat, data = xyz )
    g = gstat(g, id="elev", model=vFitgs) 
    gpredres <- predict( g, preds )
    x11()
    lp = levelplot( elev.pred ~ plon+plat, gpredres, aspect = "iso", at=zz, col.regions=color.code( "seis", zz),
      contour=FALSE, labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE)  )
    plot(lp)
  }

# now inla
  inla.setOption(scale.model.default = TRUE)  # better numerical performance of IGMRF models and less dependnence upon hyperpriors

  locs0  = as.matrix( xyz[,1:2] )
  z = xyz[,3]  # must live outside xyz for inla
  xyz$z = NULL
  xyz$b0 = 1  # intercept for inla
  
  M0.domain = inla.nonconvex.hull( locs0 )
  M0 = inla.mesh.2d (
    loc=locs0, # locations of data points
    boundary = M0.domain,
    max.edge = edge * vRange
  )
 
  kappa0 = sqrt(8) / vRange
  tau0 = 1/ ( sqrt(4*pi) * kappa0 * vPsill )

  S0 = inla.spde2.matern( M0, alpha=2, 
    B.tau = cbind(log(tau0), -1,1 ),     # parameter basis functions
    B.kappa = cbind( log(kappa0), 0, 1 ), # parameter basis functions
    theta.prior.mean = c(0,  0),    # theta1 controls variance .. vague; theta2 controls range   .. means 0
    theta.prior.prec = c(0.1, 1)  #  precisions are vague for theta1;  for range .. theta2 prec 1 ==> 95% prior prob that range is smaller than domain size
  ) 

  i <- inla.spde.make.index('i', n.spde=S0$n.spde )  

  # projection matrix A to translate from mesh nodes to data nodes
  A = inla.spde.make.A( mesh=M0, loc=locs0 )

  # data stack for occurence (PA)
  Z = inla.stack( 
      tag="data",
      data=list( z=z ) ,
      A=list(A, 1 ),
      effects=list( i=i, xyz )  # b0 is the intercept
  )

  R <- inla(  z ~ 0 + b0+ f( i, model=S0, diagonal=1e-2), 
      data=inla.stack.data(Z), 
      control.compute=list(dic=TRUE),
      control.results=list(return.marginals.random=TRUE, return.marginals.predictor=TRUE ),
      control.predictor=list(A=inla.stack.A(Z), compute=TRUE) , 
      # control.inla=list(strategy="laplace", npoints=21, stencil=7 ) ,
      verbose = FALSE
  )

  out = list()

  out$inla.summary = try( spacetime.inla.extract.parameters( R, S0, vname="i" ) )
                
  out$gstat = list( vario.empirical=vEm, vario.model=vFitgs, vario.range=vRange, 
              vario.psill=vPsill, vario.nugget=vNugget )

  if (return.inla) {
    out$inla.R = R
    out$mesh = M0 
  }
  return(out)
}


