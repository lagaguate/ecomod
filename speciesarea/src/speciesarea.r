
	### requires an update of databases entering into analysis: 
  # snow crab:  "cat" and "set.clean"
  # groundfish: "sm.base", "set"
  # and the glue function "bio.db"


# create base species area stats  ... a few hours

	
  p = list()
  p$libs = loadlibraries ( c("chron", "fields", "mgcv", "sp", "parallel")) 
  p$init.files = loadfunctions( c( "common", "bathymetry", "temperature", "habitat", "taxonomy", "bio", "speciesarea"  ) )
 
  # faster to use RAM-based data objects but this forces use only of local cpu's
  # configure SHM (shared RAM memory to be >18 GB .. in fstab .. in windows not sure how to do this?)
  p$use.bigmemory.file.backing = FALSE  
  # p$use.bigmemory.file.backing = TRUE  # file-backing is slower but can use all cpu's in a distributed cluster


  p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
  p$data.sources = c("groundfish", "snowcrab") 
  p$speciesarea.method = "glm" 
  
  p$pred.radius = 50 # km
  p$timescale = c( 0, 1, 2 ) # yr
  p$lengthscale = c( 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100, 110, 120 )  # km used in counting for rarefaction curve
  p$interpolation.distances = 25  # habitat interpolation scale
   
  p$taxa = "maxresolved"
  # p$taxa = "family.or.genera"
  # p$taxa = "alltaxa"
  
  p$season = "allseasons"

  # choose:
  # p$clusters = rep( "localhost", 1)  # if length(p$clusters) > 1 .. run in parallel
  # p$clusters = rep( "localhost", 2 )
  # p$clusters = rep( "localhost", 8 )
  p$clusters = rep( "localhost", 4 )

  # p$clusters = c( rep( "nyx.beowulf", 24), rep("tartarus.beowulf", 24), rep("kaos", 24 ) )

  p$yearstomodel = 1970:2013 # set map years separately to temporal.interpolation.redo allow control over specific years updated
  p$varstomodel = c( "C", "Z", "T", "Npred" )

  # p$mods = c("simple","simple.highdef", "time.invariant", "complex", "full" ) 
  p$mods = "complex" 
  
  p$habitat.predict.time.julian = "Sept-1" # Sept 1
 
  p$spatial.knots = 100
  p$movingdatawindow = c( -4:+4 )  # this is the range in years to supplement data to model 
  p$movingdatawindowyears = length (p$movingdatawindow)

  p$optimizer.alternate = c( "outer", "nlm" )  # first choice is bam, then this .. see GAM options



# -------------------------------------------------------------------------------------
# Run BIO.DB to update the multi-survey databases /home/jae/ecomod/bio/src/bio.r
# -------------------------------------------------------------------------------------


  # count and record rarification curves from all available data --- refresh "bio.db" ~/ecomod/bio/src/bio.r  
  p$clusters = rep( "localhost", 24 )
  speciesarea.db( DS="speciesarea.counts.redo", p=p )  # 60 MB / process  -- can use all cpus
  

  # compute species-area relationships 
  speciesarea.db( DS="speciesarea.stats.redo", p=p ) # ~ 1 minute
  speciesarea.db( DS="speciesarea.stats.filtered.redo", p=p ) # ~ 1 minute
  speciesarea.db( DS="speciesarea.stats.merged.redo", p=p ) # intermediary file for modelling and interpolation


  # create a spatial interpolation model for each variable of interest ~ 10 min
  # for Fulll model each process requires 30-40 GB 
  p = make.list( list(vars= p$varstomodel, modtype=p$mods, yrs=p$yearstomodel), Y=p ) 
  parallel.run( speciesarea.model.spatial, DS="redo", p=p ) 
  # speciesarea.model.spatial ( DS="redo", p=p ) 


  # predictive interpolation to full domain (iteratively expanding spatial extent) ~ 30 min to 1 hr / year (simple)
  p = make.list( list( yrs=p$yearstomodel, modtype=p$mods), Y=p )
  parallel.run( speciesarea.interpolate, DS="redo", p=p ) 
  # speciesarea.interpolate( DS="redo", p=p ) 


  # map everything
  p = make.list( list( vars=p$varstomodel, yrs=p$yearstomodel, modtype=p$mods ), Y=p )
  parallel.run( speciesarea.map, p=p, type="annual"  ) 
  # speciesarea.map( p=p, type="annual"  ) 


  # to do: maps and gridding in 5 and 10 year blocks ... 


