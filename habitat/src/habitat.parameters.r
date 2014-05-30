


  p = list()
  p$init.files = loadfunctions( c(
	  "common", "habitat", "substrate", "bathymetry", "speciesarea", "metabolism", 
		"sizespectrum", "speciescomposition", "temperature", "biochem", "condition" 
	) )

  p$libs = loadlibraries( "mgcv", "sp", "gstat",  "parallel", "fields", "chron" ) 
  p$taxa = "maxresolved"
  p$season = "allseasons"
  p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 ) 
  p$interpolation.nmax = 100 
   
 

  p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far

  p$yearstomodel = 1970:2013
  
  
  p$clusters = rep("localhost", detectCores() )
 # p$clusters = rep("localhost",8)
  # p$clusters = c( rep("kaos", 14), rep("tartarus", 14), rep("nyx", 14) )



