  
  # estimate metabolic demand, given size structure

  ### requires an update of databases entering into analysis: 
  # snow crab:  "cat" and "set.clean"
  # groundfish: "sm.base", "set"
  # and the glue function "bio.db" 


  # ----->  ALSO, uses the lookup function ,,, 
  #   i.e., be careful with dependency order as metabolism will 
  #   eventually need a lookup method too !!!


  p = list()

  p$libs = loadlibraries ( c("chron", "fields", "mgcv", "sp", "parallel", "snow" ))
  p$init.files = loadfunctions( c(
	  "common", "habitat", "bathymetry", "bio", "temperature", "taxonomy", "metabolism"
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
 
  p$spatial.knots = 200
  p$movingdatawindow = c( -2:+2 )  # this is the range in years to supplement data to model 
  p$movingdatawindowyears = length (p$movingdatawindow)

  p$optimizer.alternate = c( "outer", "nlm" )  # first choice is bam, then this .. see GAM options

  # p$mods = c("simple","simple.highdef", "complex", "full" )  # model types to attempt
  # p$mods = c("simple","simple.highdef" )  # model types to attempt
  p$mods = "complex"


  # prepare data
  metabolism.db( DS="metabolism.redo", p=p )
   
  
  # full model: the data ~ 14GB/ variable
  # moving 7yr window < 1GB / variable and ~ 5 min
  # RAM requirements are large and speed is slow for a full model .. using a moving time-window 
  p = make.list( list(vars= p$varstomodel, modtype=p$mods, yrs=p$yearstomodel), Y=p ) 
  parallel.run( clusters=p$clusters, n=p$nruns, metabolism.model, p=p, DS="redo" ) 
  # metabolism.model( p=p, DS="redo" ) 


  # predict data: gridded extrapolations to full domain  
  # ~ 5 GB / process
  p$clusters = c( rep( "nyx.beowulf", 12), rep("tartarus.beowulf", 12), rep("kaos", 12 ) )
  p$clusters =  "localhost"
  
  p = make.list( list( yrs=p$yearstomodel, modtype=p$mods), Y=p )
  parallel.run( clusters=p$clusters, n=p$nruns, metabolism.interpolate, p=p, DS="redo" ) 
  # metabolism.interpolate( p=p, DS="redo" ) 
  

  # map everything
  p = make.list( list(vars=p$varstomodel, yrs=p$yearstomodel, modtype=p$mods), Y=p )
  parallel.run( clusters=p$clusters, n=p$nruns, metabolism.map, p=p, type="annual"  ) 
  # metabolism.map ( p=p, type="annual" )





