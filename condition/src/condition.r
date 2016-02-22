  
  # estimate condition

  ### requires an update of databases entering into analysis: 
  # snow crab:  "cat" and "set.clean"
  # groundfish: "sm.base", "set"
  # and the glue function "bio.db" 

  p = list( project.name = "condition" )

  p$project.outdir.root = project.datadirectory( p$project.name, "analysis" ) #required for interpolations and mapping
  
  p$init.files = loadfunctions( c("spacetime", "utility", "parallel", "habitat", "bathymetry",
                                  "bio", "temperature", "taxonomy", "condition" ) )
  p$libs = RLibrary( c( "lubridate", "chron", "fields", "bigmemory", "mgcv", "sp", "parallel", "grid" , "lattice", "fields", "raster", "rgdal" ))
  p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
  p$season = "allseasons"
  p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 ) / 2 # half distances   
  
  # choose:
  # p$clusters = rep( "localhost", 24 )
  # p$clusters = c( rep("tartarus", 24), rep("kaos", 17 ) )
  p$clusters = rep("localhost", detectCores() )

#  p$varstomodel = c( "coAll", "coFish", "coElasmo", "coGadoid", "coDemersal", "coPelagic", "coSmallPelagic", "coLargePelagic", "coSmallDemersal",   "coLargeDemersal" )
  p$varstomodel = c( "coFish", "coElasmo", "coGadoid", "coDemersal", "coPelagic", 
                     "coSmallPelagic", "coLargePelagic", "coSmallDemersal",   "coLargeDemersal" )
  
  p$yearstomodel = 1970:2015
  p$spatial.knots = 100
  p$prediction.dyear = 0.75
  p$nw = 10
  p$default.spatial.domain = "canada.east"
  
  p$movingdatawindow = 0  # this signifies no moving window ... all in one model
  # p$movingdatawindow = c( -4:+4 )  # this is the range in years to supplement data to model 
  p$movingdatawindowyears = length (p$movingdatawindow)
  p$optimizer.alternate = c( "outer", "nlm" )  # first choice is default (newton), then this as a failsafe .. see GAM options

  # p$mods = c("simple","simple.highdef", "complex", "full" )  # model types to attempt
  p$modtype = "complex"

  # prepare data
  condition.db( DS="condition.redo", p=p ) # takes a minute 

# -------------------------------------------------------------------------------------
# Generic spatio-temporal interpolations and maping of data 
# using the interpolating functions and models defined in ~ecomod/habitat/src/
# -------------------------------------------------------------------------------------

  # create a spatial interpolation model for each variable of interest 
  # full model requires 5 GB per model
  # ~ 30 hrs with 2 CPUs @ 3.6 Ghz 
  # 200 hr! in 2015
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
  p$clusters = rep("localhost", detectCores() )
  p = make.list( list(vars=p$varstomodel, yrs=p$yearstomodel ), Y=p )
  parallel.run( habitat.map, p=p  ) 
  # habitat.map( p=p  ) 




