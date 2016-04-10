
# process Substrate information using SPDE /RINLA .. no GMT dependency
  
  ## NOTE:: substrate size is really only relevant for SSE/snowcrab domain right now as no 
  ##        other data source has been found/identified
  ##        but working at the size of canada.east.highres for compatibility with bathymetry 
  ##        .. might change this in future as it is also expensive in time .. but really only done once in a while, sooo...
  ## TODO:: add data collected by snow crab survey and any others for that matter 
   
  p = list( project.name = "substrate" )
  p$project.root = project.datadirectory( p$project.name )
         
  p$init.files = loadfunctions( c( "spacetime", "utility", "parallel", "bathymetry", "polygons", "substrate", "coastline" ) )
  p$libs = RLibrary( "rgdal", "maps", "mapdata", "maptools", "lattice", "parallel", "INLA", "geosphere", 
                     "sp", "raster", "colorspace" ,  "splancs", "fields",
                     "bigmemory.sri", "synchronicity", "bigmemory", "biganalytics", "bigtabulate", "bigalgebra" )
  p = spatial.parameters( type="canada.east.highres", p=p ) # highest resolution still 
   
  make.substrate.db = FALSE
  if (make.substrate.db) {
    substrate.db ( DS="substrate.initial.redo" ) # bring in Kostelev's data ... stored as a SpatialGridDataFrame
		substrate.db ( DS="lonlat.highres.redo" ) # in future .. additional data would be added here ...
  }

  interpolations.redo = FALSE
  if (interpolations.redo) {
   
      p = spacetime.parameters(p)  # load spde defaults
      p$dist.max = 75 # length scale (km) of local analysis .. for acceptance into the local analysis/model
      p$dist.mwin = 5 # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
      p$dist.pred = 0.95 # % of dist.max where **predictions** are retained (to remove edge effects)
      p$n.min = 30 # n.min/n.max changes with resolution: at p$pres=0.25, p$dist.max=25: the max count expected is 40000
      p$n.max = 5000 # numerical time/memory constraint -- anything larger takes too much time
      p$upsampling = c( 1.1, 1.2, 1.3, 1.4, 1.5 )  # local block search fractions
      p$downsampling = c( 0.25, 0.2, 0.15, 0.1, 0.075, 0.05, 0.025, 0.01 ) # local block search fractions  -- need to adjust based upon data density

      p$expected.range = 50 #+units=km km , with dependent var on log scale
      p$expected.sigma = 1e-1  # spatial standard deviation (partial sill) .. on log scale
      p$sbbox = spacetime.db( p=p, DS="statistics.box" ) # bounding box and resoltuoin of output statistics defaults to 1 km X 1 km
      # p$variables = list( Y="substrate", X=c("z", "dZ", "ddZ", "Z.rangeMode" ), LOCS=c("plon", "plat") )  
      p$variables = list( Y="substrate", X=c("z", "dZ" ), LOCS=c("plon", "plat") )  
      p$spatial.field.name = "spatial.field"  # name used in formula to index the spatal random field
      
      if (0) {
        # examine data distribution and range of var and covars 
        B = substrate.db( p=p, DS="substrate.spacetime.inputs.data" )
        summary(B)
        hist( log(B$dZ))
      }

      p$modelformula = formula( substrate ~ -1 + intercept 
        + f( inla.group(log(z+1000) ), model="rw2") 
        + f( inla.group(log(dZ+0.01)), model="rw2") 
        # + f( inla.group( log(ddZ+0.01) ), model="rw2") 
        # + f( inla.group( log(Z.rangeMode+0.01)), model="rw2" ) 
        + f( spatial.field, model=SPDE ) )
      p$spacetime.link = function( X ) { log(X)  } 
      p$spacetime.invlink = function( X ) { exp(X)  }
      p$spacetime.family = "gaussian"
      p$spacetime.outputs = c( "predictions.direct", "statistics" ) # "random.field", etc. for now: "predictions.projected" works only for simple models. This contains smooth terms 

      reset.bigmemory.objects = FALSE
      if ( reset.bigmemory.objects ) {
        # note::depends upon bathymetry
        substrate.db ( p=p, DS="substrate.spacetime.inputs.data.redo" )  
        substrate.db( p=p, DS="substrate.spacetime.inputs.prediction.redo" )
        
        # reset input data objects
        spacetime.db( p=p, DS="bigmemory.inla.inputs.data", B=substrate.db( p=p, DS="substrate.spacetime.inputs.data" ) )
        spacetime.db( p=p, DS="bigmemory.inla.inputs.prediction", B=substrate.db( p=p, DS="substrate.spacetime.inputs.prediction" ) ) # note this is the same as inputs
      
        # reset bigmemory output data objects  (e.g., if you are restarting)
        spacetime.db( p=p, DS="predictions.bigmemory.initialize" ) 
        spacetime.db( p=p, DS="statistics.bigmemory.initialize" )
        cat( paste( Sys.time(), Sys.info()["nodename"], p$project.name, p$project.root, p$spatial.domain, "\n" ),
            file=p$debug.file, append=FALSE ) # init
 
        # define boundary polygon for data to drop land etc vary parameters until it matches data ...
        p$mesh.boundary.resolution = 120 # discretization
        p$mesh.boundary.convex = -0.03  # curavature of boundary
        spacetime.db( p, DS="boundary.redo" ) 
      }
 
      if (0) {
        # cluster definition: do not use all CPU's as INLA itself is partially run in parallel
        # RAM reqiurements are a function of data density and mesh density .. currently ~ 12 GB / run
        p$clusters = "localhost"  # if serial run, send a single cluster host
        p$clusters = rep( "localhost", 6 )
        p$clusters = c( rep( "nyx", 5 ), rep ("tartarus", 5), rep("kaos", 5 ) )
        p$clusters = c( rep( "hyperion", 4 ), rep( "nyx", 10 ), rep ("tartarus", 10), rep("kaos", 10 ), rep("tethys", 2 ) ) 
      }
  
      sS = spacetime.db( p, DS="statistics.bigmemory.status" )
      sS$n.incomplete / (sS$n.problematic + sS$n.incomplete +sS$n.complete)
   
      # run the beast .. warning this will take a very long time! (weeks)
      p = make.list( list( jj=sample( sS$incomplete ) ), Y=p ) # random order helps use all cpus 
      parallel.run( spacetime.interpolate.inla, p=p ) # no more GMT dependency! :)  
      # spacetime.interpolate.inla( p=p, debugrun=TRUE )  # if testing serial process

      if (0) {
        # checking status of outputs during parallel runs:
        # bathymetry.db( DS="landmasks.create", p=p ) # run this line only if default resolution is altered 
        substrate.figures( DS="statistics", p=p ) 
        substrate.figures( DS="predictions", p=p ) 
        substrate.figures( DS="predictions.errors", p=p ) 
      }

      # save to file and clean up 
      spacetime.db( p=p, DS="predictions.redo" )  
      spacetime.db( p=p, DS="statistics.redo" )  # this also rescales results to the full domain
      spacetime.db( p=p, DS="bigmemory.inla.cleanup" )
  
  } ## End rebuild.maindatabase


  ### -----------------------------------------------------------------
  ### now move onto some useful by products: 
  ### bring together stats and predictions and any other required computations

  substrate.db( p=p, DS="substrate.spacetime.finalize.redo" )  
 
  # as the interpolation process is so expensive, regrid based off the above run
  # if you want more, will need to add to the list and modify the selection criteria
  substrate.db( p=p, DS="spde_complete.redo", grids.new=c( "canada.east.highres", "canada.east", "SSE", "snowcrab", "SSE.mpa" ) ) 

  # test outputs/ access methods
  # plot( substrate.db( p, DS="spde_complete", return.format="brick" )$substrate ) # raster brick
  # spplot( substrate.db( p, DS="spde_complete", return.format="sp" ), "substrate" ) # spatial points/grid data frame
   

