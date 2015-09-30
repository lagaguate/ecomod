

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


  ### END GMT-based methods
  


  ### START INLA-based methods

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
   
    p$dist.max = 50 # length scale (km) of local analysis .. for acceptance into the local analysis/model
    p$dist.mwin = 1 # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
    p$dist.pred = 0.90 # % of dist.max where **predictions** are retained (to remove edge effects)
   
    ## this changes with resolution: at p$pres=0.25 and a p$dist.max=25: the max count expected is 40000
    p$n.min = 100
    p$n.max = 25000 # numerical time/memory constraint

    # the following parameters are for inside and outside ... do not make them exact multiples as this seems to make things hang ..
    p$inla.mesh.max.edge = c(  0.025,   0.04 )    # proportion of 2*p$dist.max or equivalent: c(inside,outside)
    p$inla.mesh.offset   = c(  0.025,   0.05 )   # how much to extend inside and outside of boundary: proportion of dist.max
    p$inla.mesh.cutoff   = c(  0.005,   0.05 )    ## min distance allowed between points: proportion of dist.max 

    p$inla.alpha = 2 # bessel function curviness
    p$inla.nsamples = 5000 # posterior similations 
    p$expected.range = 50 # km , with dependent var on log scale
    p$expected.sigma = 1e-1  # spatial standard deviation (partial sill) .. on log scale

    p$predict.in.one.go = FALSE # use false, one go is very very slow and a resource expensive method
        
    p$modelformula = formula( ydata ~ -1 + intercept + depth + slope + curvature + f( spatial.field, model=S0 ) )
      
    p$spacetime.link = function( X ) { log(X) + 1000 } 
    p$spacetime.invlink = function( X ) { exp(X) - 1000  }


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
      return( p$spacetime.invlink( s$latent[i_intercept,1] + s$latent[ i_depth,1] + s$latent[ i_slope,1] + s$latent[ i_spatial.field,1] ) )
    }
    
    reset.input = FALSE
    if (reset.input) {
      # depends upon bathymetry
      substrate.db ( p=p, DS="substrate.spacetime.input.redo" )  # Warning: req ~ 15 min, 30 GB RAM (2015, Jae)
      spacetime.db( p=p, DS="bigmemory.inla.reset.input", B=substrate.db( p=p, DS="substrate.spacetime.input" ) )
    }
 
    p$debug.file = file.path( ecomod.workdirectory, "inla.debug.out" )
   
    reset.output = FALSE
    if (reset.output) {
      spacetime.db( p=p, DS="bigmemory.inla.reset.output" ) # create/reset bigmemory output data objects  
      cat( paste( Sys.time(), Sys.info()["nodename"], p$project.name, p$project.root, p$spatial.domain, "\n" ), file=p$debug.file, append=FALSE ) # init
    }

    # cluster definition
    # do not use all CPU's as INLA itself is partially run in parallel
    # RAM reqiurements are a function of data density and mesh density .. currently ~ 12 GB / run
    # p$clusters = "localhost"  # if serial run, send a single cluster host
    p$clusters = c( rep( "hyperion", 4 ), rep( "nyx", 5 ), rep ("tartarus", 5), rep("kaos", 5 ) )
    nS = spacetime.db( p, DS="statistics.bigmemory.size" )
    p = make.list( list( jj=sample( 1:nS ) ), Y=p ) # random order helps use all cpus 
    p = parallel.run( spacetime.interpolate.inla, p=p ) # no more GMT dependency! :)  
    
    if (0) {
      p = spacetime.db( p=p, DS="bigmemory.inla.filenames" )
      S = attach.big.matrix(p$descriptorfile.S , path=p$tmp.datadir ) 
      i = which( is.na( S[,3] ) )
      length(i)
       
      j = which( S[,3]==0 ) # not yet done
      length(j)
      p = make.list( list( jj=sample( j ) ), Y=p ) 

      P = attach.big.matrix(p$descriptorfile.P , path=p$tmp.datadir )
      pps  =  expand.grid( plons=p$plons, plats=p$plats)
      levelplot(  P[,2]  ~plons+plats, pps , aspect="iso" )
    }

    spacetime.plot( p=p, "predictions.mean.bigmemory" ) # directly from bigmatrix objects
    
    spacetime.db( p=p, DS="predictions.redo" )  
    spacetime.db( p=p, DS="statistics.redo" )
    spacetime.db( p=p, DS="bigmemory.inla.cleanup" )

  }


