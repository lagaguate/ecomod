  
  # estimate condition

  ### requires an update of databases entering into analysis: 
  # snow crab:  "cat" and "set.clean"
  # groundfish: "sm.base", "set"
  # and the glue function "bio.db" 


  # ----->  ALSO, uses the lookup function ,,, 


  p = list()
  p$init.files = loadfunctions( c( "common", "habitat", "bathymetry", "bio", "temperature", "taxonomy", "condition" ) )
  p$libs = loadlibraries( c("chron", "fields", "mgcv", "sp", "parallel", "grid" , "lattice" ))
	p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
#    p$taxa = "alltaxa"   # do not use any other category
  p$season = "allseasons"
  p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 )  
  
  # choose:
  # p$clusters = rep( "localhost", 1)  # if length(p$clusters) > 1 .. run in parallel
  # p$clusters = rep( "localhost", 2 )
  # p$clusters = rep( "localhost", 8 )
  # p$clusters = rep( "localhost", 24 )
  p$clusters = c( rep( "nyx.beowulf", 24), rep("tartarus.beowulf", 24), rep("kaos", 24 ) )
  # p$clusters = c( rep( "kaos.beowulf", 6), rep("nyx.beowulf", 24))
  # p$clusters = c( rep("tartarus.beowulf", 24), rep("kaos", 17 ) )

  p$varstomodel = c( 
    "coAll", "coFish", "coElasmo", 
    "coGadoid", "coDemersal", "coPelagic", 
    "coSmallPelagic", "coLargePelagic", "coSmallDemersal", 
    "coLargeDemersal" )
  
  p$yearstomodel = 1970:2012
  p$habitat.predict.time.julian = "Sept-1" # Sept 1
 
  p$spatial.knots = 100
  p$movingdatawindow = c( -4:+4 )  # this is the range in years to supplement data to model 
  p$movingdatawindowyears = length (p$movingdatawindow)

  p$optimizer.alternate = c( "outer", "nlm" )  # first choice is bam, then this .. see GAM options




  # p$mods = c("simple","simple.highdef", "complex", "full" )  # model types to attempt
  # p$mods = c("simple","simple.highdef" )  # model types to attempt
  p$mods = "complex"

  # prepare data
  condition.db( DS="condition.redo", p=p )
 

  # model the data ~ 14GB/ variable
  p = make.list( list(vars= p$varstomodel, modtype=p$mods, yrs=p$yearstomodel), Y=p ) 
  parallel.run( condition.model, p=p, DS="redo" ) 


  # predict data: gridded extrapolations to full domain  
  p = make.list( list( yrs=p$yearstomodel, modtype=p$mods), Y=p )
  parallel.run( condition.interpolate, p=p, DS="redo" ) 
  

  # map everything
  p = make.list( list(vars=p$varstomodel, yrs=p$yearstomodel, modtype=p$mods), Y=p )
  parallel.run( condition.map, p=p, type="annual"  ) 
  # condition.map ( p=p, type="annual" )

