  # Bathymetry data 
  # processing bathymetry data with RINLA  .. no GMT dependency 

  # initialize bigmemory data objects
  p=list()
  p$init.files = loadfunctions( c( "spacetime", "utility", "parallel", "bathymetry" ) )
  p$libs = RLibrary( "rgdal", "maps", "mapdata", "maptools", "lattice", "parallel", "INLA", "geosphere", "sp", "raster", "colorspace" ,
    "bigmemory.sri", "synchronicity", "bigmemory", "biganalytics", "bigtabulate", "bigalgebra", "splancs")
  
  p$project.name = "bathymetry"
  p$project.root = project.datadirectory( p$project.name )
  
  p = spatial.parameters( type="canada.east.highres", p=p ) ## highres = 0.5 km discretization
  
  redo.bathymetry.rawdata = FALSE
  if ( redo.bathymetry.rawdata ) { 
    bathymetry.db ( p=spatial.parameters( type="canada.east", p=p ), DS="z.lonlat.rawdata.redo", additional.data=c("snowcrab", "groundfish") )
  }

  p = spacetime.parameters(p)  # load spde defaults

  p$dist.max = 100 # length scale (km) of local analysis .. for acceptance into the local analysis/model
  p$dist.mwin = 5 # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
  p$dist.pred = 0.95 # % of dist.max where **predictions** are retained (to remove edge effects)
 
  ## this changes with resolution: at p$pres=0.25 and a p$dist.max=25: the max count expected is 40000
  p$n.min = 30
  p$n.max = 7500 # numerical time/memory constraint

  p$expected.range = 50 # km , with dependent var on log scale
  p$expected.sigma = 1e-1  # spatial standard deviation (partial sill) .. on log scale

  p$sbbox = spacetime.db( p=p, DS="statistics.box" ) # bounding box and resoltuoin of output statistics defaults to 1 km X 1 km

  p$debug.file = file.path( ecomod.workdirectory, "inla.debug.out" )


  p$modelformula = formula( ydata ~ -1 + intercept + f( spatial.field, model=SPDE ) ) # SPDE is the spatial covariance model .. defined in spacetime.interpolate.inla (below)

  p$spatial.field.name = "spatial.field"  # name used in formula to index the spatal random field

  p$spacetime.link = function( X ) { log(X + 1000) }  ## data range is from -383 to 5467 m .. 1000 shifts all to positive valued as this will operate on the logs
  p$spacetime.invlink = function( X ) { exp(X) - 1000 }

  p$spacetime.family = "gaussian"
  
  # if not in one go, then the value must be reconstructed from the correct elements:  
  p$spacetime.posterior.extract = function(s, rnm) { 
    # rnm are the rownames that will contain info about the indices ..
    # optimally the grep search should only be done once but doing so would 
    # make it difficult to implement in a simple structure/manner ... 
    # the overhead is minimal relative to the speed of modelling and posterior sampling
    i_intercept = grep("intercept", rnm, fixed=TRUE ) # matching the model index "intercept" above .. etc
    i_spatial.field = grep("spatial.field", rnm, fixed=TRUE )
    return(  s$latent[i_intercept,1] + s$latent[ i_spatial.field,1] )
  }
 

  reset.input = FALSE
  if (reset.input) {
    # faster if you do this step on kaos (the fileserver)
    bathymetry.db ( p, DS="bathymetry.spacetime.input.redo" )  # Warning: req ~ 15 min, 40 GB RAM (2015, Jae)
    spacetime.db( p=p, DS="bigmemory.inla.reset.input", B=bathymetry.db( p=p, DS="bathymetry.spacetime.input" ) )
  }

  
  reset.output = FALSE
  if (reset.output) {
    spacetime.db( p=p, DS="bigmemory.inla.reset.output" ) # create/reset bigmemory output data objects  
    cat( paste( Sys.time(), Sys.info()["nodename"], p$project.name, p$project.root, p$spatial.domain, "\n" ), file=p$debug.file, append=FALSE ) # init
  }

  # cluster definition
  # do not use all CPU's as INLA itself is partially run in parallel
  # RAM reqiurements are a function of data density and mesh density .. currently ~ 12 GB / run
  # p$clusters = "localhost"  # if serial run, send a single cluster host
  # p$clusters = c( "hyperion",  "nyx", "tartarus", "kaos", "tethys" ) 
  p$clusters = rep( "localhost", 6 )
  # p$clusters = c( rep( "hyperion", 6 ), rep( "nyx", 24 ), rep ("tartarus", 24), rep("kaos", 24 ), rep("tethys", 6 ) )
  nS = spacetime.db( p, DS="statistics.bigmemory.size" )
    
  p = make.list( list( jj=sample( 1:nS ) ), Y=p ) # random order helps use all cpus 
  
  # spacetime.interpolate.inla( p=p, debugrun=TRUE ) 
  parallel.run( spacetime.interpolate.inla, p=p ) # no more GMT dependency! :)  
 


  if (0) {
    # low level check of results
    p = spacetime.db( p=p, DS="bigmemory.inla.filenames" )
    
    # predictions
    S = attach.big.matrix(p$descriptorfile.S , path=p$tmp.datadir ) 
 
    # problematic and/or no data (i.e., land) and skipped
    i = which( is.nan( S[,3] ) )
    length(i)
     
    # not yet completed
    j = which( is.na( S[,3] ) ) 
    length(j)

    # completed 
    k = which( is.finite (S[,3])  ) # not yet done
    length(k)



  # world coastline data base

  
  # generate landmask
    if (index.redo) {
      V = data.frame( cbind(plon=S[,1], plat=S[,2]) )
      V = SpatialPoints( planar2lonlat( V, proj.type=p$internal.crs )[, c("lon", "lat" )], CRS("+proj=longlat +datum=WGS84") ) 
      landmask( lonlat=V, db="worldHires",regions=c("Canada", "US"), ylim=c(36,53), xlim=c(-72,-45),
      tag="statistics" )
    }
  
    p = spacetime.db( p=p, DS="bigmemory.inla.filenames" )
    
    # predictions
    S = attach.big.matrix(p$descriptorfile.S , path=p$tmp.datadir ) 
 

    x11()
    p$spatial.domain="canada.east"  # force isobaths to work in levelplot
    datarange = log( c( 5, 800 ))
    dr = seq( datarange[1], datarange[2], length.out=150)
    oc = landmask( db="worldHires", regions=c("Canada", "US"), return.value="not.land", tag="statistics" )
    levelplot( log(S[oc,3])  ~ S[oc,1] + S[oc,2] , aspect="iso", at=dr, col.regions=color.code( "seis", dr) ,
      contour=FALSE, labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE), cex=2,
      panel = function(x, y, subscripts, ...) {
        panel.levelplot (x, y, subscripts, aspect="iso", rez=c(5,5), ...)
        #coastline
        cl = landmask( return.value="coast.lonlat",  ylim=c(36,53), xlim=c(-72,-45) )
        cl = lonlat2planar( data.frame( cbind(lon=cl$x, lat=cl$y)), proj.type=p$internal.crs )
        panel.xyplot( cl$plon, cl$plat, col = "black", type="l", lwd=0.8 )
        zc = isobath.db( p=p, depths=c( 300 ) )  
        zc = lonlat2planar( zc, proj.type=p$internal.crs) 
        panel.xyplot( zc$plon, zc$plat, col = "gray", pch=".", cex=0.1 )
      }
    ) 
    p$spatial.domain="canada.east.highres"

    
    
    
    #predictions 

    # generate landmask
    if (index.redo) {
      V = SpatialPoints( planar2lonlat( pps, proj.type=p$internal.crs )[, c("lon", "lat" )], CRS("+proj=longlat +datum=WGS84") ) 
      landmask( lonlat=V, db="worldHires",regions=c("Canada", "US"), ylim=c(36,53), xlim=c(-72,-45), tag="predictions" )
    }
    
    p = spacetime.db( p=p, DS="bigmemory.inla.filenames" )
  
    P = attach.big.matrix(p$descriptorfile.P , path=p$tmp.datadir )
    pps  =  expand.grid( plons=p$plons, plats=p$plats)

    x11()
    p$spatial.domain="canada.east"  # force isobaths to work in levelplot
    datarange = log( c( 5, 4000 ))
    dr = seq( datarange[1], datarange[2], length.out=100)
    oc = landmask( db="worldHires", regions=c("Canada", "US"), return.value="not.land", tag="predictions" )
    levelplot( log( P[oc,2] ) ~ plons + plats, pps[oc,], aspect="iso", main=NULL, at=dr, col.regions=rev(color.code( "seis", dr)) ,
      contour=FALSE, labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE),
        panel = function(x, y, subscripts, ...) {
        panel.levelplot (x, y, subscripts, aspect="iso", rez=c(1,1), ...)
        #coastline
        cl = landmask( return.value="coast.lonlat",  ylim=c(36,53), xlim=c(-72,-45)  )
        cl = lonlat2planar( data.frame( cbind(lon=cl$x, lat=cl$y)), proj.type=p$internal.crs )
        panel.xyplot( cl$plon, cl$plat, col = "steelblue", type="l", lwd=0.8 )
   #     zc = isobath.db( p=p, depths=c(200, 400 ) )  
   #     zc = lonlat2planar( zc, proj.type=p$internal.crs) 
   #     panel.xyplot( zc$plon, zc$plat, col = "steelblue", pch=".", cex=0.1 )
      }
    )
    p$spatial.domain="canada.east.highres"


    # prediction error 
    x11()
     datarange = log ( c( 2, 50 ))
     dr = seq( datarange[1], datarange[2], length.out=150)
     levelplot( log( P[oc,3])~ plons + plats, pps[oc,], aspect="iso", main="mean", at=dr, col.regions=rev(color.code( "seis", dr)) , contour=FALSE, labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )

    # redo incomplete
    p = make.list( list( jj=sample( j ) ), Y=p ) 
    p = parallel.run( spacetime.interpolate.inla, p=p ) # no more GMT dependency! :)  

  }

  spacetime.plot( p=p, "predictions.mean.bigmemory" ) # directly from bigmatrix objects
  
  # save to file and clean up 
  spacetime.db( p=p, DS="predictions.redo" )  
  spacetime.db( p=p, DS="statistics.redo" )  # this also rescales results to the full domain
  spacetime.db( p=p, DS="bigmemory.inla.cleanup" )

  # compute slope and curvature and then assimilate results in bathymetry
  bathymetry.db( p=p, DS="bathymetry.spacetime.finalize.redo" )  

  # as the interpolation process is so expensive, regrid based off the above run
  --- TODO --- 
  bathymetry.db( p=p, DS="finalized", regrid=c( "SSE", "snowcrab", "mpa" ) ) # if you want more, will need to add to the list and modify the selection criteria



