  
  # estimate metabolic demand, given size structure

  ### requires an update of databases entering into analysis: 
  # snow crab:  "cat" and "set.clean"
  # groundfish: "sm.base", "set"
  # and the glue function "bio.db" 


  # ----->  ALSO, uses the lookup function ,,, 
  #   i.e., be careful with dependency order as metabolism will 
  #   eventually need a lookup method too !!!
  
  p = list( project.name = "metabolism" )

  p$project.outdir.root = project.datadirectory( p$project.name, "analysis" )

  p$libs = RLibrary ( c("chron", "fields", "bigmemory", "mgcv", "sp", "parallel", "rgdal" ))
  p$init.files = loadfunctions( c(
	  "spacetime", "utility", "parallel", "habitat", "bathymetry", "bio", "temperature", "taxonomy", "metabolism"
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
  p$yearstomodel = 1970:2015
  p$habitat.predict.time.julian = "Sept-1" # Sept 1
  p$default.spatial.domain = "canada.east"
  p$prediction.dyear = 0.75
  p$nw = 10
 
  p$spatial.knots = 100
  p$interpolation.distances =  25 # for interpolation of habitat vars
  p$prediction.dyear = 0.75
  p$nw = 10

  p$optimizer.alternate = c( "outer", "nlm" )  # first choice is bam, then this .. see GAM options

  p$modtype = "complex"

  # prepare data
  metabolism.db( DS="metabolism.redo", p=p )
   
 

# -------------------------------------------------------------------------------------
# Generic spatio-temporal interpolations and maping of data 
# using the interpolating functions and models defined in ~ecomod/habitat/src/
# -------------------------------------------------------------------------------------

   # create a spatial interpolation model for each variable of interest 
  # full model requires 5-6 GB 
  
  p$varstomodel = c( "smr", "Pr.Reaction" , "Ea", "A" )  # on hyperion
  
  p$varstomodel = c( "zn", "zm", "qn", "qm", "mass" )  # on kaos

  ## WARNING takes 16GB / run!!
  p$clusters = rep("localhost", length( p$varstomodel ) ) 
  p = make.list( list(vars= p$varstomodel ), Y=p )  #  
  parallel.run( habitat.model, DS="redo", p=p ) 
  # habitat.model ( DS="redo", p=p ) 
  
  # p$varstomodel = c( "mr", "smr", "Pr.Reaction" , "Ea", "A", "zn", "zm", "qn", "qm", "mass", "len"  )
 
  # predictive interpolation to full domain (iteratively expanding spatial extent)
  # ~ 5 GB /process required so on a 64 GB machine = 64/5 = 12 processes 
  p$clusters = rep("localhost", 10) # 6 GB / process
  p = make.list( list( yrs=p$yearstomodel ), Y=p )
  parallel.run( habitat.interpolate, p=p, DS="redo" ) 
 
   
  # map everything
  p$clusters = rep("localhost", detectCores() )
  p = make.list( list(vars=p$varstomodel, yrs=p$yearstomodel ), Y=p )
  parallel.run( habitat.map, p=p  ) 
  # habitat.map( p=p  ) 






