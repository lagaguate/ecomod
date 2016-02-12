
  # analysis and spatial database of normalised size spectrum, average size and condition

	
	### requires an update of databases entering into analysis: 
  # snow crab:  "cat" and "set.clean"
  # groundfish: "sm.base", "set"
  # and the glue function "bio.db"


  # create base species area stats  ... a few hours

  p = list()
 
  p$libs = RLibrary ( c( "chron", "fields", "bigmemory", "mgcv", "sp", "parallel", "rgdal" )) 
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
  # p$clusters = rep( "localhost", 2 )
  # p$clusters = rep( "localhost", 8 )
  # p$clusters = rep( "localhost", 4 )
  # p$clusters = c( rep( "nyx.beowulf", 14), rep("tartarus.beowulf", 14), rep("kaos", 13 ) )
  # p$clusters = c( rep( "nyx.beowulf", 24), rep("tartarus.beowulf", 24), rep("kaos", 24 ) )
  # p$clusters = rep("localhost", detectCores() )
  # p$clusters = rep(c("kaos", "nyx", "tartarus"), 2)
  


  p$yearstomodel = 1970:2015 # set map years separately to temporal.interpolation.redo allow control over specific years updated
  
  # for spatial interpolation of nss stats
  p$varstomodel = c( "nss.rsquared", "nss.df", "nss.b0", "nss.b1", "nss.shannon" )
  
  p$modtype =  "complex"  
  p$habitat.predict.time.julian = "Sept-1" # Sept 1

  p$spatial.knots = 100
  
  p$movingdatawindow = 0  # this signifies no moving window ... all in one model
  # p$movingdatawindow = c( -4:+4 )  # this is the range in years to supplement data to model 
  p$movingdatawindowyears = length (p$movingdatawindow)

  p$optimizer.alternate = c( "outer", "nlm" )  # first choice is bam, then this .. see GAM options


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

  #required for interpolations and mapping 
  p$project.name = "sizespectrum"
  p$project.outdir.root = project.datadirectory( p$project.name, "analysis" )


  if (p$movingdatawindow == 0 ) { 
      # create a spatial interpolation model for each variable of interest 
      # full model requires 5 GB
      p$clusters = rep("localhost", detectCores() )
      p = make.list( list(vars= p$varstomodel ), Y=p )  # no moving window 
      parallel.run( habitat.model, DS="redo", p=p ) 
      #habitat.model ( DS="redo", p=p ) 
 
  
      # predictive interpolation to full domain (iteratively expanding spatial extent)
      # ~ 5 GB /process required so on a 64 GB machine = 64/5 = 12 processes 
      p$clusters = rep("localhost", detectCores() )
      p = make.list( list(vars= p$varstomodel ), Y=p )  # no moving window 
      parallel.run( habitat.interpolate, p=p, DS="redo" ) 
      # habitat.interpolate( p=p, DS="redo" ) 
  
  } else {
      p = make.list( list(vars= p$varstomodel, yrs=p$yearstomodel ), Y=p ) 
      parallel.run( habitat.model, DS="redo", p=p ) 
      # habitat.model ( DS="redo", p=p ) 

      # predictive interpolation to full domain (iteratively expanding spatial extent)
      p = make.list( list(yrs=p$yearstomodel ), Y=p ) 
      parallel.run( habitat.interpolate, p=p, DS="redo" ) 
      # habitat.interpolate( p=p, DS="redo" ) 
  
  }

 
  # map everything
  # p$clusters = rep( "localhost", 8 )
  p$clusters = c( rep( "nyx", 20), rep("tartarus", 20), rep("kaos", 20 ) )
  p = make.list( list(vars=p$varstomodel, yrs=p$yearstomodel ), Y=p )
  parallel.run( habitat.map, p=p ) 
  # habitat.map( p=p ) 




