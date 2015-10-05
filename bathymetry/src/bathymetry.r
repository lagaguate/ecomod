# -------------------------------------------------------------------------------------
# Bathymetry data --- warning this depends upon GMT (Generic Mapping Tools) 
  
  p=list()
  p$init.files = loadfunctions( c( "spacetime", "utility", "parallel", "bathymetry" ) )
  p$libs = RLibrary( "rgdal", "lattice", "parallel" )


  # ------------------
  # glue all data sources (spherical coords) 
  # ... right now this is about 17 GB in size when expanded .... SLOW .... 
  # and it takes about 52+ GB RAM (due to addition of Greenlaw's DEM )
  # run on servers only unless your machine can handle it
  redo.bathymetry.rawdata = FALSE
  if ( redo.bathymetry.rawdata ) { 
		p = spatial.parameters( type="canada.east", p=p )
    bathymetry.db ( p, DS="z.lonlat.rawdata.redo", additional.data=c("snowcrab", "groundfish") )
 }


  # ------------------
  # GMT-based methods:
  # NOTE: too many clusters will overload the system as data files are large ~(11GB RAM required to block) 
  # for the high resolution maps .. the temporary files can be created deleted/overwritten files 
  # in the temporary drives 
  redo.isobaths = FALSE
  if (redo.isobaths) {
    area = c( "snowcrab", "SSE", "ecnasap", "canada.east" ) 
    for (sp in area) {
      p$spatial.domain = sp
      p = spatial.parameters( p=p )
      p = gmt.parameters(p)  # interpolation parameters ... currently using GMT to interpolate bathymetry
      # override defaults in gmt.parameters as additional ones are used by other systems including lattice
      p$isobaths = c( 0, seq(50, 450, by=50), seq( 500, 1000, by=100 )  ) #override defaults 
      p = make.list( list( depths=p$isobaths ), Y=p )
      p$clusters = rep( "localhost", 1 )  
      #isobath.db( p=p, DS="redo" ) 
      parallel.run( isobath.db,  p=p, DS="redo" ) 	
    }
  }


  # ------------------
  # intermediary base maps with location definitions, annotations and isobaths ... to speed up PS map production .. only for GMT maps
  redo.basemap.gmt = FALSE  
  if ( redo.basemap.gmt ) {
    area = c( "snowcrab", "SSE", "ecnasap", "canada.east" ) 
    for (sp in area) {
      p$spatial.domain = sp
      p = spatial.parameters( p=p )
      p = gmt.parameters(p)  # interpolation parameters ... currently using GMT's isobaths whcih are specified in gmt.parameters
      # or if you want to override the isobaths plotted define them here (but make sure they were created in the previous step)
      # p$isobaths = c( seq(50, 450, by=100)  )
      gmt.basemap(p)
    }
  }

  
  # ------------------
  # prepare finalised bathymetry data for use in ecomod
  complete.bathymetry.db = FALSE
  areas = c( "canada.east", "SSE" ) # only two are currently used  
  for ( sp in areas ) {
    p = spatial.parameters( type=sp, p=p )
    p = gmt.parameters(p)
    bathymetry.db ( p, DS="prepare.intermediate.files.for.dZ.ddZ" )  # uses GMT's math functions ...
		bathymetry.db ( p, DS="Z.redo" )
		bathymetry.db ( p, DS="dZ.redo" )
		bathymetry.db ( p, DS="ddZ.redo" )
    bathymetry.db ( p, DS="baseline.redo" ) # additional filtering of areas and or depth to reduce file size
    bathymetry.db ( p, DS="complete.redo" ) # glue all together 
	
  }

 
  # ------------------
  # "snowcrab" subsets do exist but are simple subsets of SSE 
  # so only the lookuptable below is all that is important as far as bathymetry is concerned
  # both share the same initial domains + resolutions
 	p = spatial.parameters( type="snowcrab", p=p )
  bathymetry.db ( p, DS="baseline.redo" ) # additional filtering of areas and or depth to reduce file size
  bathymetry.db( DS="lookuptable.sse.snowcrab.redo" ) 
 



  # ------------------
  ## a few lattice-based maps: for SSE only right now
  p = spatial.parameters( type="SSE" )
  x = bathymetry.db ( p, DS="baseline" )
  
	snowcrab.area=F
	if (snowcrab.area) {
		# this is used below
		sc = intersect( 
				which( x$plon< 990 & x$plon > 220  & x$plat< 5270 & x$plat > 4675 ) ,
				filter.region.polygon( x[, c("plon", "plat") ], "cfaall", planar=T) 
		)
		x = x[sc,]
	}
	
	x$z =log( x$z )
  
  outdir = file.path(project.datadirectory("bathymetry","maps"), p$spatial.domain) 

  dr = quantile( x$z, probs=c(0.005, 0.995))
  datarange = seq(dr[1], dr[2], length.out=100)
  cols = color.code( "blue.black", datarange )
  outfn = "depth"
  annot = "ln ( Depth; m )"
  map( xyz=x[,c("plon", "plat", "z")], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
    fn=outfn, loc=outdir, at=datarange , col.regions=cols, spatial.domain=p$spatial.domain )
  
  
  x = bathymetry.db ( p, DS="dZ.planar" )
	if (snowcrab.area) x = x[sc,]
  dr = quantile( x$dZ, probs=c(0.005, 0.995))
  datarange = seq(dr[1], dr[2], length.out=100)
  cols = color.code( "blue.black", datarange )
  outfn = "slope"
  annot = "ln ( Slope; m/m )"
  map( xyz=x[ ,c("plon", "plat", "dZ")], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
    fn=outfn, loc=outdir, at=datarange , col.regions=cols , spatial.domain=p$spatial.domain )

 
  x = bathymetry.db ( p, DS="ddZ.planar" )
	if (snowcrab.area) x = x[sc,]
  dr = quantile( x$ddZ, probs=c(0.005, 0.995))
  datarange = seq(dr[1], dr[2], length.out=100)
  cols = color.code( "blue.black", datarange )
  outfn = "curvature"
  annot = "ln ( Curvature; m/m/m )"
  map( xyz=x[,c("plon", "plat", "ddZ")], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
    fn=outfn, loc=outdir, at=datarange , col.regions=cols, spatial.domain=p$spatial.domain )



  ### END GMT-based methods


  # --------------------------


  ### START INLA-based methods

  
  process.bathymetry.data.via.inla = FALSE
  if (process.bathymetry.data.via.inla) {
    ## ----- Adaptive estimation method (test) :
    # processing bathymetry data with RINLA  .. no GMT dependency 
  
    # initialize bigmemory data objects
    p=list()
    p$init.files = loadfunctions( c( "spacetime", "utility", "parallel", "bathymetry" ) )
    p$libs = RLibrary( 
      "rgdal", "lattice", "parallel", "INLA", "geosphere", "sp", "raster", "colorspace" ,
      "bigmemory.sri", "synchronicity", "bigmemory", "biganalytics", "bigtabulate", "bigalgebra", "splancs")
    
    p$project.name = "bathymetry"
    p$project.root = project.datadirectory( p$project.name )
    
    p = spatial.parameters( type="canada.east.highres", p=p ) ## highres = 0.5 km discretization
    
    redo.bathymetry.rawdata = FALSE
    if ( redo.bathymetry.rawdata ) { 
      bathymetry.db ( p=spatial.parameters( type="canada.east", p=p ), DS="z.lonlat.rawdata.redo", additional.data=c("snowcrab", "groundfish") )
    }

    p$dist.max = 75 # length scale (km) of local analysis .. for acceptance into the local analysis/model
    p$dist.mwin = 5 # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
    p$dist.pred = 0.95 # % of dist.max where **predictions** are retained (to remove edge effects)
   
    ## this changes with resolution: at p$pres=0.25 and a p$dist.max=25: the max count expected is 40000
    p$n.min = 30
    p$n.max = 6000 # numerical time/memory constraint

    # the following parameters are for inside and outside ... do not make them exact multiples as this seems to make things hang ..
    p$inla.mesh.max.edge = c(  0.025,   0.04 )    # proportion of 2*p$dist.max or equivalent: c(inside,outside) -- must be positive valued
    p$inla.mesh.offset   = c( - 0.025,  - 0.05 )   # how much to extend inside and outside of boundary: proportion of dist.max .. neg val = proportion
    p$inla.mesh.cutoff   = c( - 0.05,   - 0.5 )    ## min distance allowed between points: proportion of dist.max ; neg val = proportion

    p$inla.mesh.hull.radius = c( -0.04, - 0.08 ) ## radius of boundary finding algorythm ; neg val = proportion

    p$inla.mesh.hull.resolution = 125  ## resolution for discretization to find boundary

    p$spacetime.noise = 0.001  # add a little noise to coordinates to prevent a race condition

    p$inla.alpha = 2 # bessel function curviness
    p$inla.nsamples = 5000 # posterior similations 
    p$expected.range = 50 # km , with dependent var on log scale
    p$expected.sigma = 1e-1  # spatial standard deviation (partial sill) .. on log scale

    p$predict.in.one.go = FALSE # use false, one go is very very slow and a resource expensive method
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
    p$clusters = c( rep( "hyperion", 5 ), rep( "nyx", 15 ), rep ("tartarus", 15), rep("kaos", 15 ), rep("tethys", 5 ) )
    nS = spacetime.db( p, DS="statistics.bigmemory.size" )
      
    p = make.list( list( jj=sample( 1:nS ) ), Y=p ) # random order helps use all cpus 
    
    # spacetime.interpolate.inla( p=p, debugrun=TRUE ) 
    parallel.run( spacetime.interpolate.inla, p=p ) # no more GMT dependency! :)  
    
    if (0) {
      # low level check of results
      p = spacetime.db( p=p, DS="bigmemory.inla.filenames" )
      
      # predictions
      S = attach.big.matrix(p$descriptorfile.S , path=p$tmp.datadir ) 
     
      # S[,3] is the range estimate
      x11()
      datarange = log( c( 5, 800 ))
      dr = seq( datarange[1], datarange[2], length.out=150)
      levelplot( log(S[,3])  ~ S[,1] + S[,2] , aspect="iso", at=dr, col.regions=color.code( "seis", dr) ,
        contour=FALSE, labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE), cex=2  )  

      # problematic and/or no data (i.e., land) and skipped
      i = which( is.nan( S[,3] ) )
      length(i)
       
      # not yet completed
      j = which( is.na( S[,3] ) ) 
      length(j)

      # completed 
      k = which( is.finite (S[,3])  ) # not yet done
      length(k)

      #predictions 
      P = attach.big.matrix(p$descriptorfile.P , path=p$tmp.datadir )
      pps  =  expand.grid( plons=p$plons, plats=p$plats)
   
      datarange = log( c( 5, 5000 ))
      dr = seq( datarange[1], datarange[2], length.out=150)
      levelplot( log( P[,2] ) ~ plons + plats, pps, aspect="iso", main="mean", at=dr, col.regions=rev(color.code( "seis", dr)) ,
        contour=FALSE, labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
    
      # redo incomplete
      p = make.list( list( jj=sample( j ) ), Y=p ) 
      p = parallel.run( spacetime.interpolate.inla, p=p ) # no more GMT dependency! :)  
 
    }

    spacetime.plot( p=p, "predictions.mean.bigmemory" ) # directly from bigmatrix objects
    
    spacetime.db( p=p, DS="predictions.redo" )  
    spacetime.db( p=p, DS="statistics.redo" )
    spacetime.db( p=p, DS="bigmemory.inla.cleanup" )

    bathymetry.db( p=p, DS="inla.finalize.redo" )  # create a completed bathymtry db for use with ecomod

  }


