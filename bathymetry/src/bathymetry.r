  
# Bathymetry data: processing bathymetry data with RINLA  .. no GMT dependency 
# warning: this will take weeks as it is an iterative process

  p = list( project.name = "bathymetry" )

  p$project.root = project.datadirectory( p$project.name )
         
  p$init.files = loadfunctions( c( "spacetime", "utility", "parallel", "bathymetry", "coastline", "polygons" )  )
  p$libs = RLibrary( c( "rgdal", "maps", "mapdata", "maptools", "lattice", "parallel", "INLA",
    "geosphere", "sp", "raster", "colorspace" ,  "splancs", "fields",
    "bigmemory.sri", "synchronicity", "bigmemory", "biganalytics", "bigtabulate", "bigalgebra" ) )
  
  # default (= only supported resolution of 0.5 km discretization)  .. do NOT change 
  # use "spde_complete" to project/downscale/upscale onto other grids/resolutions
  p = spatial.parameters( type="canada.east.highres", p=p ) 
  
  rebuild.maindatabase = FALSE
  if (rebuild.maindatabase) {

      p = spacetime.parameters(p)  # load spde defaults
      p$dist.max = 75 # length scale (km) of local analysis .. for acceptance into the local analysis/model
      p$dist.mwin = 5 # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
      p$dist.pred = 0.95 # % of dist.max where **predictions** are retained (to remove edge effects)
      p$n.min = 30 # n.min/n.max changes with resolution: at p$pres=0.25, p$dist.max=25: the max count expected is 40000
      p$n.max = 8000 # numerical time/memory constraint -- anything larger takes too much time
      p$upsampling = c( 1.1, 1.2, 1.5, 2, 2.5, 3 )  # local block search fractions
      p$downsampling = c( 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.25, 0.2 ) # local block search fractions  -- need to adjust based upon data density
      p$expected.range = 50 #+units=km km , with dependent var on log scale
      p$expected.sigma = 1e-1  # spatial standard deviation (partial sill) .. on log scale
      p$sbbox = spacetime.db( p=p, DS="statistics.box" ) # bounding box and resoltuoin of output statistics defaults to 1 km X 1 km
      p$variables = list( Y="z", LOCS=c("plon", "plat") )
      
      p$spatial.field.name = "spatial.field"  # name used in formula to index the spatal random field
      p$modelformula = formula( z ~ -1 + intercept + f( spatial.field, model=SPDE ) ) # SPDE is the spatial covariance model .. defined in spacetime.interpolate.inla (below)
      p$spacetime.link = function( X ) { log(X + 1000) }  ## data range is from -100 to 5467 m .. 1000 shifts all to positive valued by one -order of magnitude 
      p$spacetime.invlink = function( X ) { exp(X) - 1000 }
      p$spacetime.family = "gaussian"
      p$spacetime.outputs = c( "predictions.projected", "statistics" ) # "random.field", etc.
      
      
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
      
      reset.bigmemory.objects = FALSE
      if ( reset.bigmemory.objects ) {
         # prepare data for modelling and prediction:: faster if you do this step on kaos (the fileserver)
         bathymetry.db ( p=spatial.parameters( type="canada.east", p=p ), DS="z.lonlat.rawdata.redo", additional.data=c("snowcrab", "groundfish") )
         bathymetry.db( p=p, DS="bathymetry.spacetime.inputs.data.redo" )  # Warning: req ~ 15 min, 40 GB RAM (2015, Jae) data to model (with covariates if any)
         bathymetry.db( p=p, DS="bathymetry.spacetime.inputs.prediction.redo" ) # i.e, pred locations (with covariates if any )

         # transfer data into spacetime methods as bigmemory objects
         spacetime.db( p=p, DS="bigmemory.inla.inputs.data", B=bathymetry.db( p=p, DS="bathymetry.spacetime.inputs.data" ) )
         spacetime.db( p=p, DS="bigmemory.inla.inputs.prediction", B=bathymetry.db(p=p, DS="bathymetry.spacetime.inputs.prediction" )) ## just locations, no covars
      
         # reset bigmemory output data objects  (e.g., if you are restarting)
         spacetime.db( p=p, DS="predictions.bigmemory.initialize" ) 
         spacetime.db( p=p, DS="statistics.bigmemory.initialize" )
         cat( paste( Sys.time(), Sys.info()["nodename"], p$project.name, p$project.root, p$spatial.domain, "\n" ),
            file=p$debug.file, append=FALSE ) # init
        
        # define boundary polygon for data .. this trims the prediction/statistics locations to speed things up a little ..
        p$mesh.boundary.resolution = 150
        p$mesh.boundary.convex = -0.025
        spacetime.db( p, DS="boundary.redo" ) # ~ 5 min 
        # bathymetry.db( DS="landmasks.create", p=p ) # re-run only if default resolution is altered ... very slow 1 hr?  
      }

      if (0) {
        # cluster definition
        # do not use all CPU's as INLA itself is partially run in parallel
        # RAM reqiurements are a function of data density and mesh density .. currently ~ 12 GB / run
        p$clusters = "localhost"  # if serial run, send a single cluster host
        p$clusters = rep( "localhost", 5 )
        p$clusters = c( rep( "nyx", 5 ), rep ("tartarus", 5), rep("kaos", 5 ), rep("tethys", 2) )
        p$clusters = c( rep( "hyperion", 4 ), rep( "nyx", 10 ), rep ("tartarus", 10), rep("kaos", 10 ), rep("tethys", 2 ) ) 
      }
      
      # run the beast .. warning this will take a very long time! (weeks)
      
      sS = spacetime.db( p, DS="statistics.bigmemory.status" )
      sS$n.incomplete / ( sS$n.problematic + sS$n.incomplete + sS$n.complete)
   
      p = make.list( list( jj=sample( sS$incomplete ) ), Y=p ) # random order helps use all cpus 
      parallel.run( spacetime.interpolate.inla, p=p ) # no more GMT dependency! :)  
      # spacetime.interpolate.inla( p=p, debugrun=TRUE )  # if testing serial process

      if (0) {
        # for checking status of outputs during parallel runs:
        bathymetry.figures( DS="statistics", p=p ) 
        bathymetry.figures( DS="predictions", p=p ) 
        bathymetry.figures( DS="predictions.error", p=p )

        p = spacetime.db( p=p, DS="bigmemory.inla.filenames" )
        S = bigmemory::attach.big.matrix(p$descriptorfile.S, path=p$tmp.datadir)  # statistical outputs
        hist(S[,1] )
        o = which( S[,1] > 600 )
        S[o,] = NA
        S[sS$problematic,] = NA
        o = which( S[,1] < 10 )
        S[o,] = NA
      }

      # save to file 
      spacetime.db( p=p, DS="predictions.redo" )  
      spacetime.db( p=p, DS="statistics.redo" )  # this also rescales results to the full domain
       
      # clean up bigmemory files
      spacetime.db( p=p, DS="bigmemory.inla.cleanup" )

  } ## End rebuild.maindatabase


  ### -----------------------------------------------------------------
  ### now move onto some useful by products: complete databse, polygons for isobaths, etc..
 
  # bring together stats and predictions and any other required computations: slope and curvature
  bathymetry.db( p=p, DS="bathymetry.spacetime.finalize.redo" )  

  # as the interpolation process is so expensive, regrid/upscale/downscale based off the above run
  # if you want more, will need to add to the list and modify the selection criteria
  # NOTE:  bathymetry.db( p=p, DS="complete" ) is now a synonym for:
  #        bathymetry.db( p=p, DS="spde_complete" )
  
  new.grids = c( "canada.east.highres", "canada.east", "SSE", "SSE.mpa" , "snowcrab")
  bathymetry.db( p=p, DS="spde_complete.redo", grids.new=new.grids ) 

  # filtering of areas and or depth to reduce file size, in planar coords only
  for (domain in new.grids){ 
    print(domain)
    bathymetry.db ( p=spatial.parameters( type=domain ), DS="baseline.redo" ) 
  }

  # "snowcrab" subsets do exist but are simple subsets of SSE 
  # so only the lookuptable below is all that is important as far as bathymetry is concerned
  # both share the same initial domains + resolutions
   
  bathymetry.db( p=spatial.parameters( type="snowcrab" ), DS="lookuptable.sse.snowcrab.redo" ) # indices to map SSE to snowcrab
 
  # to recreate new polygons, run the following:
  bathyclines.redo = FALSE
  depths = c( 0, 10, 20, 50, 75, 100, 200, 250, 300, 400, 500, 600, 700, 750, 800, 900, 
               1000, 1200, 1250, 1400, 1500, 1750, 2000, 2500, 3000, 4000, 5000 )
  if( bathyclines.redo ) {
    # note these polygons are created at the resolution specified in p$spatial.domain .. 
    # which by default is very high ("canada.east.highres" = 0.5 km .. p$pres ). 
    # For lower one specify an appropriate p$spatial.domain
    plygn = isobath.db( p=p, DS="isobath.redo", depths=depths  )
  }
  
  plygn = isobath.db( p=p, DS="isobath", depths=depths  )

  coast = coastline.db( xlim=c(-68,-52), ylim=c(41,50), no.clip=TRUE )
  plot( coast, col="transparent", border="steelblue2" , xlim=c(-68,-52), ylim=c(41,50),  xaxs="i", yaxs="i", axes=TRUE )  # ie. coastline
  lines( plygn[ as.character(c( 100, 200, 300 ))], col="gray90" ) # for multiple polygons
  lines( plygn[ as.character(c( 500, 1000))], col="gray80" ) # for multiple polygons
  # plot( plygn, xlim=c(-68,-52), ylim=c(41,50))  # all isobaths commented as it is slow ..


  # or to get in projected (planar) coords as defined by p$spatial domain
  plygn = isobath.db( p=p, DS="isobath", depths=c(100) , crs=p$internal.crs ) # as SpatialLines
  plot(plygn)

  plygn_aslist = coordinates( plygn) 
  plot( 0,0, type="n", xlim=c(-200,200), ylim=c(-200,200)  )
  lapply( plygn_aslist[[1]], points, pch="." )

  plygn_as_xypoints = coordinates( as( plygn, "SpatialPoints") )# ... etc...
  plot(plygn_as_xypoints, pch=".",  xaxs="i", yaxs="i", axes=TRUE)


  # a few plots :
  
  p = spatial.parameters( type="canada.east.highres", p=p ) 
  b = bathymetry.db( p=p, DS="spde_complete" ) 
  
  vn = "z"
  u = b[[vn]] [ is.finite( b[[vn]]) ]
  if (length( u) > 0 ) out = x[u]

  mypalette = colorRampPalette(c("darkblue","blue3", "green", "yellow", "orange","red3", "darkred"), space = "Lab")(100)
  mybreaks = classIntervals( u, n=length(mypalette), style="quantile")$brks
  
  depths = c( 100, 200, 300, 400, 500 )
  plygn = isobath.db( p=p, DS="isobath", depths=depths  )

  sab = as( Polygon( coords=polygon.db( id="aoi.st.anns") ), "SpatialPolygons" )
  sab = spTransform( sab, crs( plygn) )

  sp.layout= list( sab, plygn )
  
  spplot( b, vn, col.regions=mypalette, main=vn, sp.layout=coastLayout, col="transparent" )



