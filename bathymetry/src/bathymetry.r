  # Bathymetry data: processing bathymetry data with RINLA  .. no GMT dependency 

  p = list( project.name = "bathymetry" )
  p$project.root = project.datadirectory( p$project.name )
         
  p$init.files = loadfunctions( c( "spacetime", "utility", "parallel", "bathymetry" ) )
  p$libs = RLibrary( "rgdal", "maps", "mapdata", "maptools", "lattice", "parallel", "INLA", "geosphere", 
                     "sp", "raster", "colorspace" ,  "splancs", "fields",
                     "bigmemory.sri", "synchronicity", "bigmemory", "biganalytics", "bigtabulate", "bigalgebra" )
  
  ## highres = 0.5 km discretization  .. do NOT change .. instead use "spde_complete" below to project onto other grids
  p = spatial.parameters( type="canada.east.highres", p=p ) 
  
  redo.bathymetry.rawdata = FALSE
  if ( redo.bathymetry.rawdata ) { 
    bathymetry.db ( p=spatial.parameters( type="canada.east", p=p ), DS="z.lonlat.rawdata.redo", additional.data=c("snowcrab", "groundfish") )
  }

  p = spacetime.parameters(p)  # load spde defaults
  p$dist.max = 100 # length scale (km) of local analysis .. for acceptance into the local analysis/model
  p$dist.mwin = 5 # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
  p$dist.pred = 0.95 # % of dist.max where **predictions** are retained (to remove edge effects)
  p$n.min = 30 # n.min/n.max changes with resolution: at p$pres=0.25, p$dist.max=25: the max count expected is 40000
  p$n.max = 7500 # numerical time/memory constraint -- anything larger takes too much time
  p$expected.range = 50 #+units=km km , with dependent var on log scale
  p$expected.sigma = 1e-1  # spatial standard deviation (partial sill) .. on log scale
  p$sbbox = spacetime.db( p=p, DS="statistics.box" ) # bounding box and resoltuoin of output statistics defaults to 1 km X 1 km

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
  p$clusters = rep( "localhost", 6 )
  # p$clusters = c( rep( "hyperion", 4 ), rep( "nyx", 10 ), rep ("tartarus", 10), rep("kaos", 10 ), rep("tethys", 2 ) )
  sS = spacetime.db( p, DS="statistics.bigmemory.status" )
  sS$n.incomplete / (sS$n.problematic + sS$n.incomplete +sS$n.complete)

  p = make.list( list( jj=sample( sS$incomplete ) ), Y=p ) # random order helps use all cpus 
  parallel.run( spacetime.interpolate.inla, p=p ) # no more GMT dependency! :)  
  # spacetime.interpolate.inla( p=p, debugrun=TRUE )  # if serial process


 # for debugging:
  bathymetry.db( DS="landmasks.create", p=p ) # need do only once, unless resolution is being changed
 # bathymetry.figures( DS="statistics", p=p ) # need do only once, unless resolution is being changed
 # bathymetry.figures( DS="predictions", p=p ) # need do only once, unless resolution is being changed
 # bathymetry.figures( DS="predictions.errors", p=p ) # need do only once, unless resolution is being changed
  
  # save to file 
  spacetime.db( p=p, DS="predictions.redo" )  
  spacetime.db( p=p, DS="statistics.redo" )  # this also rescales results to the full domain
  
  # clean up bigmemory files
  spacetime.db( p=p, DS="bigmemory.inla.cleanup" )

  # compute slope and curvature and then assimilate results in bathymetry.db
  bathymetry.db( p=p, DS="bathymetry.spacetime.finalize.redo" )  

  # as the interpolation process is so expensive, regrid based off the above run
  # if you want more, will need to add to the list and modify the selection criteria
  bathymetry.db( p=p, DS="spde_complete.redo", grids.new=c( "canada.east.highres", "canada.east", "SSE", "snowcrab", "SSE.mpa" ) ) 

  # test outputs/ access methods
  # plot( bathymetry.db( p, DS="spde_complete", return.format="brick" )$z ) # raster brick
  # spplot( bathymetry.db( p, DS="spde_complete"), "z" ) # default return value is a spatial points/grid data frame

  
  ### NOTE:  bathymetry.db ( p, DS="complete" ) is a synonym for   bathymetry.db( p=p, DS="spde_complete" )
	
  # filtering of areas and or depth to reduce file size, in planar coords only
  for (domain in c("canada.east.highres", "canada.east", "SSE", "SSE.mpa", "snowcrab" ) ) {
    bathymetry.db ( p=spatial.parameters( type=domain ), DS="baseline" ) 
  }

  # "snowcrab" subsets do exist but are simple subsets of SSE 
  # so only the lookuptable below is all that is important as far as bathymetry is concerned
  # both share the same initial domains + resolutions
   
  bathymetry.db( p=spatial.parameters( type="snowcrab" ), DS="lookuptable.sse.snowcrab.redo" ) # indices to map SSE to snowcrab
 

  coast = isobath.db( p=p, DS="coastLine.redo", return.lonlat=TRUE ) # flatten into one
  coast = isobath.db( p=p, DS="coastPolygon.redo", return.lonlat=TRUE )
  plygn = isobath.db( p=p, DS="isobath.redo", 
    depths=c(0, 10, 20, 50, 75, 100, 200, 300, 400, 500, 600, 700, 800, 900, 
             1000, 1200, 1400, 1500, 2000, 2500, 3000, 4000, 5000 ), return.lonlat=TRUE  )



