

  p = list()
  p$init.files = 	loadfunctions( c("spacetime", "utility", "substrate", "bathymetry" ) ) 
  p$libs = RLibrary( "maptools" , "rgdal" )


	# --------------------------------------
  # create the main database
  # some require upto 1.2 GB RAM, ~ 5 min
  # no need to run again unless the substrate data file is updated ... 

  make.substrate.db = FALSE
  if (make.substrate.db) {
    substrate.db ( DS="substrate.initial.redo" ) # stored as a SpatialGridDataFrame
		substrate.db ( p=spatial.parameters( type="SSE" ), DS="lonlat.highres.redo" )
		for ( j in c( "SSE", "canada.east" ) ) {  # sse and snowcrab have same domains
			p = spatial.parameters( type=j )
			substrate.db ( p, DS="lonlat.interpolated.redo" )
			substrate.db ( p, DS="lonlat.redo" )
			substrate.db ( p, DS="planar.redo" )
		}
  }


  # --------------------------------------
  # substrate = substrate.db( p, DS="lonlat.highres" ) # or lonlat to refresh, planar or planar.saved 
  # library(lattice)
  # levelplot( log(grainsize) ~ lon + lat, substrate, main = "ln( grainsize; mm )", aspect="iso")
  
  # load the imported data in a data.frame format in a snow crab-consistent coordinates framework
  p = spatial.parameters( type="SSE" )
		
  substrate = substrate.db( p, DS="planar" ) # or lonlat to refresh, planar or planar.saved 
  i = which( substrate$plon< 990 &  substrate$plon > 220  &
             substrate$plat< 5270 &  substrate$plat > 4675 
  )
  substrate = substrate[ i, ]
  substrate$grainsize =log( substrate$grainsize )
  inside = filter.region.polygon( substrate[, c("plon", "plat") ], "cfaall", planar=T)
  datacols = c("plon", "plat", "grainsize")
  datarange = seq(-5,3, length.out=50)
  cols = color.code( "blue.black", datarange )
  outfn = "substrate.grainsize"
  annot = "ln ( Grain size; mm )"
  map( xyz=substrate[inside,datacols], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
    fn=outfn, loc=file.path( project.datadirectory("substrate"), "R"), at=datarange , col.regions=cols )


  

  process.substrate.data.via.inla = FALSE
  if (process.substrate.data.via.inla) {
    ## ----- Adaptive estimation method (test) :
    # processing substrate data with RINLA  .. no GMT dependency 
    # initialize bigmemory data objects
     
    p=list()
    p$init.files = loadfunctions( c( "spacetime", "utility", "parallel", "bathymetry", "substrate" ) )
 
    p$libs = RLibrary( 
        "rgdal", "lattice", "parallel", "INLA", "geosphere", "sp", "raster", "colorspace" ,
        "bigmemory.sri", "synchronicity", "bigmemory", "biganalytics", "bigtabulate", "bigalgebra")
      
    p$project.name = "substrate"

    p$project.root = project.datadirectory( p$project.name )
      
    p = spatial.parameters( type="canada.east.highres", p=p ) ## highres = 0.5 km discretization
     
    p$dist.max = 25 # length scale (km) of local analysis .. for acceptance into the local analysis/model
    p$dist.mwin = 1 # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
    p$dist.pred = 0.95 # % of dist.max where **predictions** are retained (to remove edge effects)
     
    ## this changes with resolution: at p$pres=0.25 and a p$dist.max=25: the max count expected is 40000
    p$n.min = 100
    p$n.max = 15000 # numerical time/memory constraint

    p$inla.mesh.offset   = p$pres * c( 5, 25 ) # km
    p$inla.mesh.max.edge = p$pres * c( 5, 25 ) # km
    p$inla.mesh.cutoff   = p$pres * c( 2.5, 25 ) # km 

    p$inla.alpha = 2 # bessel function curviness
    p$inla.nsamples = 5000 # posterior similations 
    p$expected.range = 50 # km , with dependent var on log scale
    p$expected.sigma = 1e-1  # spatial standard deviation (partial sill) .. on log scale

    p$Yoffset = 1000 ## data range is from XXX to YYY .. shift all to positive valued as this will operate on the logs

    p$predict.in.one.go = FALSE # use false, one go is very very slow and a resource expensive method
        
    p$modelformula = formula( substrate ~ -1 + intercept + depth + slope + curvature + f( spatial.field, model=S0 ) )
     
    # if not in one go, then the value must be reconstructed from the correct elements:  
    p$spacetime.posterior.extract = function(s, rnm) { 
      # rnm are the rownames that will contain info about the indices ..
      # optimally the grep search should only be done once but doing so would 
      # make it difficult to implement in a simple structure/manner ... 
      # the overhead is minimal relative to the speed of modelling and posterior sampling
      i_intercept = grep("intercept", rnm, fixed=TRUE ) # matching the model index "intercept" above .. etc
      i_depth = grep("depth", rnm, fixed=TRUE ) # matching the model index "intercept" above .. etc
      i_slope = grep("slope", rnm, fixed=TRUE ) # matching the model index "intercept" above .. etc
      i_spatial.field = grep("spatial.field", rnm, fixed=TRUE ) 
      
      exp( s$latent[i_intercept,1] + s$latent[ i_depth,1] + s$latent[ i_slope,1]  
          + s$latent[ i_spatial.field,1] ) - p$Yoffset 
    }
   
    spacetime.db( p=p, DS="bigmemory.inla.reset.input", 
                  B=bathymetry.db( p=p, DS="z.lonlat.discretized" ) )
    spacetime.db( p=p, DS="bigmemory.inla.reset.output" ) # create/reset bigmemory output data objects  

    # cluster definition
    # do not use all CPU's as INLA itself is partially run in parallel
    # RAM reqiurements are a function of data density and mesh density .. currently ~ 12 GB / run
    # p$clusters = "localhost"  # if serial run, send a single cluster host
    p$clusters = c( rep( "hyperion", 4 ), rep( "nyx", 5 ), rep ("tartarus", 5), rep("kaos", 5 ) )
    nS = spacetime.db( p, DS="statistics.bigmemory.size" )
    p = make.list( list( jj=sample( 1:nS ) ), Y=p ) # random order helps use all cpus 
    p = parallel.run( spacetime.interpolate.inla, p=p ) # no more GMT dependency! :)  
    
    spacetime.plot( p=p, "predictions.mean.bigmemory" ) # directly from bigmatrix objects
 
    spacetime.db( p=p, DS="predictions.redo" ) # finalize predictions  
    spacetime.db( p=p, DS="statistics.redo" )  # finalize statistics
    spacetime.db( p=p, DS="bigmemory.inla.cleanup" )

  }









