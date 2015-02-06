  
  # estimate condition

  ### requires an update of databases entering into analysis: 
  # snow crab:  "cat" and "set.clean"
  # groundfish: "sm.base", "set"
  # and the glue function "bio.db" 


  # ----->  ALSO, uses the lookup function ,,, 


  p = list()
 
  #required for interpolations and mapping 
  p$project.name = "condition"
  p$project.outdir.root = project.directory( p$project.name, "analysis" )
  
  p$init.files = loadfunctions( c("spatialmethods", "utility", "parallel", "habitat", "bathymetry",
                                  "bio", "temperature", "taxonomy", "condition" ) )
  p$libs = RLibrary( c("chron", "fields", "bigmemory", "mgcv", "sp", "parallel", "grid" , "lattice" ))
  p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
  p$season = "allseasons"
  p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 ) / 2 # half distances   
  
  # choose:
  # p$clusters = rep( "localhost", 1)  # if length(p$clusters) > 1 .. run in parallel
  # p$clusters = rep( "localhost", 2 )
  # p$clusters = rep( "localhost", 8 )
  # p$clusters = rep( "localhost", 24 )
  # p$clusters = c( rep( "nyx.beowulf", 24), rep("tartarus.beowulf", 24), rep("kaos", 24 ) )
  # p$clusters = c( rep( "kaos.beowulf", 6), rep("nyx.beowulf", 24))
  # p$clusters = c( rep("tartarus.beowulf", 24), rep("kaos", 17 ) )
 
  p$clusters = rep("localhost", detectCores() )

  p$varstomodel = c( "coAll", "coFish", "coElasmo", "coGadoid", "coDemersal", "coPelagic", 
                     "coSmallPelagic", "coLargePelagic", "coSmallDemersal",   "coLargeDemersal" )
  
  p$yearstomodel = 1970:2014
  p$habitat.predict.time.julian = "Sept-1" # Sept 1
 
  p$spatial.knots = 100
  
  
  p$movingdatawindow = 0  # this signifies no moving window ... all in one model
  # p$movingdatawindow = c( -4:+4 )  # this is the range in years to supplement data to model 
  p$movingdatawindowyears = length (p$movingdatawindow)

  p$optimizer.alternate = c( "outer", "nlm" )  # first choice is bam, then this as a failsafe .. see GAM options


  # p$mods = c("simple","simple.highdef", "complex", "full" )  # model types to attempt
  p$modtype = "complex"


  # prepare data
  condition.db( DS="condition.redo", p=p )

# -------------------------------------------------------------------------------------
# Generic spatio-temporal interpolations and maping of data 
# using the interpolating functions and models defined in ~ecomod/habitat/src/
# -------------------------------------------------------------------------------------

  # create a spatial interpolation model for each variable of interest 
  # full model requires 5 GB per model
  # ~ 30 hrs with 2 CPUs @ 3.6 Ghz
  if (p$movingdatawindow == 0 ) { 
    p = make.list( list(vars= p$varstomodel ), Y=p )  # no moving window 
    parallel.run( habitat.model, DS="redo", p=p ) 
    # habitat.model ( DS="redo", p=p ) 
 
    # predictive interpolation to full domain (iteratively expanding spatial extent)
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
  p = make.list( list(vars=p$varstomodel, yrs=p$yearstomodel ), Y=p )
  parallel.run( habitat.map, p=p  ) 
  # habitat.map( p=p  ) 




