
  # analysis and spatial database of normalised size spectrum, average size and condition

	
	### requires an update of databases entering into analysis: 
  # snow crab:  "cat" and "set.clean"
  # groundfish: "sm.base", "set"
  # and the glue function "bio.db"


  # create base species area stats  ... a few hours

  p = list( project.name = "sizespectrum" )
  p$project.outdir.root = project.datadirectory( p$project.name, "analysis" )

  p$libs = RLibrary ( c( "lubridate", "chron", "fields", "bigmemory", "mgcv", "sp", "parallel",  "grid" , "lattice", "fields", "raster", "rgdal" )) 
  p$init.files = loadfunctions( c( "spacetime", "utility", "parallel", "bathymetry", "temperature",  "habitat",  "taxonomy", "groundfish", "bio", "sizespectrum"  ) )
  
  # faster to use RAM-based data objects but this forces use only of local cpu's
  # configure SHM (shared RAM memory to be >18 GB .. in fstab .. in windows not sure how to do this?)
  p$use.bigmemory.file.backing = FALSE  
  # p$use.bigmemory.file.backing = TRUE  # file-backing is slower but can use all cpu's in a distributed cluster

  p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
  p$taxa = "maxresolved"
  # p$taxa = "family.or.genera"
  # p$taxa = "alltaxa"
  
  p$season = "allseasons"

  # choose:
  # p$clusters = rep( "localhost", 1)  # if length(p$clusters) > 1 .. run in parallel
  # p$clusters = rep(c("kaos", "nyx", "tartarus"), 2)
  p$clusters = rep("localhost", detectCores() )
  
  p$yearstomodel = 1970:2015 # set map years separately to temporal.interpolation.redo allow control over specific years updated
  
  # for spatial interpolation of nss stats
  p$varstomodel = c( "nss.rsquared", "nss.df", "nss.b0", "nss.b1", "nss.shannon" )
  
  p$modtype =  "complex"  
  p$spatial.knots = 100
  p$prediction.dyear = 0.75
  p$nw = 10
  p$default.spatial.domain = "canada.east"
  
  p$optimizer.alternate = c( "outer", "nlm" )  # first choice is newton, then this .. see GAM options

  p$timescale = c( 0,1,2,5 ) # yr  
  p$interpolation.distances =  25 # for interpolation of habitat vars
  p$prediction.dyear = 0.75
  p$nw = 10

  # for generation of nss
  p$ntimescale = length(p$timescale)
  p$nss.distances=50  # km
  p$nss.stimes= 50 # days
  p$nss.type ="mass"
  p$nss.base =2
  p$nss.taxa = "all"

  if (p$nss.type=="mass") p$nss.bins = bins.df( "gf.mass", p$nss.base )
  if (p$nss.type=="len")  p$nss.bins = bins.df( "gf.len",  p$nss.base )

  
# -------------------------------------------------------------------------------------
# Run BIO.DB to update the multi-survey databases /home/jae/ecomod/bio/src/bio.r
# -------------------------------------------------------------------------------------


  sizespectrum.db( DS="sizespectrum.by.set.redo", p=p ) #MG takes 1 minute
  sizespectrum.db( DS="sizespectrum.stats.redo", p=p )  #MG took 20 minutes
   
  p$clusters = rep("localhost", detectCores() )
  sizespectrum.db( DS="sizespectrum.redo", p=p )  # all point data to be interpolated #MG took 5 minutes


# -------------------------------------------------------------------------------------
# Generic spatio-temporal interpolations and maping of data 
# using the interpolating functions and models defined in ~ecomod/habitat/src/
# -------------------------------------------------------------------------------------
  p$clusters = rep("localhost", length( p$varstomodel ) ) 
  p = make.list( list(vars= p$varstomodel ), Y=p ) 
  parallel.run( habitat.model, DS="redo", p=p ) 
  # habitat.model ( DS="redo", p=p ) 

  # predictive interpolation to full domain (iteratively expanding spatial extent)
  p$clusters = rep("localhost", 10) # 6 GB / process
  p = make.list( list(vars= p$varstomodel, yrs=p$yearstomodel ), Y=p ) 
  parallel.run( habitat.interpolate, p=p, DS="redo" ) 
  # habitat.interpolate( p=p, DS="redo" ) 


  # map everything
  # p$clusters = rep( "localhost", 8 )
  p$clusters = rep("localhost", detectCores() )
  p = make.list( list(vars=p$varstomodel, yrs=p$yearstomodel ), Y=p )
  parallel.run( habitat.map, p=p ) 
  # habitat.map( p=p ) 




