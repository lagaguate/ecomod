
  # analysis and spatial database of normalised size spectrum, average size and condition

	
	### requires an update of databases entering into analysis: 
  # snow crab:  "cat" and "set.clean"
  # groundfish: "sm.base", "set"
  # and the glue function "bio.db"


  # create base species area stats  ... a few hours

  p = list()
 
  p$libs = loadlibraries ( c("chron", "fields", "mgcv", "sp", "parallel")) 
  p$init.files = loadfunctions( c( "common", "bathymetry", "temperature", "habitat", "taxonomy", "bio", "sizespectrum"  ) )

  p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
  p$taxa = "maxresolved"
  # p$taxa = "family.or.genera"
  # p$taxa = "alltaxa"
  
  p$season = "allseasons"

  # choose:
  # p$clusters = rep( "localhost", 1)  # if length(p$clusters) > 1 .. run in parallel
  # p$clusters = rep( "localhost", 2 )
  # p$clusters = rep( "localhost", 8 )
  p$clusters = rep( "localhost", 4 )
  # p$clusters = c( rep( "nyx.beowulf", 14), rep("tartarus.beowulf", 14), rep("kaos", 13 ) )
  # p$clusters = c( rep( "nyx.beowulf", 24), rep("tartarus.beowulf", 24), rep("kaos", 24 ) )

  p$yearstomodel = 1970:2013 # set map years separately to temporal.interpolation.redo allow control over specific years updated
  
  # for spatial interpolation of nss stats
  p$varstomodel = c( "nss.rsquared", "nss.df", "nss.b0", "nss.b1", "nss.shannon", "nss.evenness", "nss.Hmax")
  # p$mods = c("simple","simple.highdef", "complex", "full" ) 
  # p$mods = c("simple","simple.highdef") 
  p$mods =  "complex" 
  p$habitat.predict.time.julian = "Sept-1" # Sept 1

  p$spatial.knots = 100
  p$movingdatawindow = c( -2:+2 )  # this is the range in years to supplement data to model 
  p$movingdatawindowyears = length (p$movingdatawindow)

  p$optimizer.alternate = c( "outer", "nlm" )  # first choice is bam, then this .. see GAM options


  p$timescale = c( 0,1,2,5 ) # yr  
  p$interpolation.distances =  25 # for interpolation of habitat vars


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


  sizespectrum.db( DS="sizespectrum.by.set.redo", p=p ) 
  sizespectrum.db( DS="sizespectrum.stats.redo", p=p )  
  sizespectrum.db( DS="sizespectrum.stats.filtered.redo", p=p )
  sizespectrum.db( DS="sizespectrum.stats.merged.redo", p=p )


  # create a spatial interpolation model for each variable of interest 
  # full model requires 30-40 GB ! no parallel right now for that .. currently running moving time windowed approach
  p = make.list( list(vars= p$varstomodel, mods=p$mods, yrs=p$yearstomodel ), Y=p ) 
  parallel.run( clusters=p$clusters, n=p$nruns, sizespectrum.model.spatial, DS="redo", p=p ) 
  # sizespectrum.model.spatial ( DS="redo", p=p ) 
 

  # predictive interpolation to full domain (iteratively expanding spatial extent)
  # ~ 5 GB /process required so on a 64 GB machine = 64/5 = 12 processes 
  p = make.list( list( yrs=p$yearstomodel, modtype=p$mods), Y=p )
  parallel.run( clusters=p$clusters, n=p$nruns, sizespectrum.interpolate, p=p, DS="redo" ) 
  # sizespectrum.interpolate( p=p, DS="redo" ) 


  # map everything
  p = make.list( list(v=p$varstomodel, y=p$yearstomodel, modtype=p$mods ), Y=p )
  # parallel.run( clusters=p$clusters, n=p$nruns, sizespectrum.map, p=p, type="annual"  ) 
  sizespectrum.map( p=p, type="annual"  ) 


  # to do: maps and gridding in 5 and 10 year blocks ... 



