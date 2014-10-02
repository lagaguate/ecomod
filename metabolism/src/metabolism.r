  
  # estimate metabolic demand, given size structure

  ### requires an update of databases entering into analysis: 
  # snow crab:  "cat" and "set.clean"
  # groundfish: "sm.base", "set"
  # and the glue function "bio.db" 


  # ----->  ALSO, uses the lookup function ,,, 
  #   i.e., be careful with dependency order as metabolism will 
  #   eventually need a lookup method too !!!


  p = list()

  p$libs = RLibrary ( c("chron", "fields", "bigmemory", "mgcv", "sp", "parallel" ))
  p$init.files = loadfunctions( c(
	  "spatialmethods", "utility", "parallel", "habitat", "bathymetry", "bio", "temperature", "taxonomy", "metabolism"
	) )
  
	p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
  p$taxa = "alltaxa"   # do not use any other category
  p$season = "allseasons"
  p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 )  
  
  # p$clusters = rep( "localhost", 1)  # if length(p$clusters) > 1 .. run in parallel
  # p$clusters = rep( "localhost", 2 )
   p$clusters = rep("localhost", detectCores() )
  # p$clusters = c( rep( "nyx.beowulf", 24), rep("tartarus.beowulf", 24), rep("localhost", 24 ) )

  p$varstomodel = c( "mr", "smr", "Pr.Reaction" , "Ea", "A", "zn", "zm", "qn", "qm", "mass", "len"  )
  
  p$yearstomodel = 1970:2013
  p$habitat.predict.time.julian = "Sept-1" # Sept 1
 
  p$spatial.knots = 100
  
  p$movingdatawindow = 0  # this signifies no moving window ... all in one model
  # p$movingdatawindow = c( -4:+4 )  # this is the range in years to supplement data to model 
  p$movingdatawindowyears = length (p$movingdatawindow)

  p$optimizer.alternate = c( "outer", "nlm" )  # first choice is bam, then this .. see GAM options

  p$modtype = "complex"

  # prepare data
  metabolism.db( DS="metabolism.redo", p=p )
   
 

# -------------------------------------------------------------------------------------
# Generic spatio-temporal interpolations and maping of data 
# using the interpolating functions and models defined in ~ecomod/habitat/src/
# -------------------------------------------------------------------------------------

  #required for interpolations and mapping 
  p$project.name = "metabolism"
  p$project.outdir.root = project.directory( p$project.name, "analysis" )


  # create a spatial interpolation model for each variable of interest 
  # full model requires 30-40 GB ! no parallel right now for that .. currently running moving time windowed approach
  if (p$movingdatawindow == 0 ) { 
    p = make.list( list(vars= p$varstomodel ), Y=p )  # no moving window 
  } else {
    p = make.list( list(vars= p$varstomodel, yrs=p$yearstomodel ), Y=p ) 
  }
  parallel.run( habitat.model, DS="redo", p=p ) 
  # habitat.model ( DS="redo", p=p ) 
 

  # predictive interpolation to full domain (iteratively expanding spatial extent)
  # ~ 5 GB /process required so on a 64 GB machine = 64/5 = 12 processes 
  p = make.list( list( yrs=p$yearstomodel ), Y=p )
  parallel.run( habitat.interpolate, p=p, DS="redo" ) 
  # habitat.interpolate( p=p, DS="redo" ) 


  # map everything
  p = make.list( list(vars=p$varstomodel, yrs=p$yearstomodel ), Y=p )
  parallel.run( habitat.map, p=p  ) 
  # habitat.map( p=p  ) 






