


  p = list()
  p$init.files = loadfunctions( c(
	  "spatialmethods", "utility", "parallel", "habitat", "substrate", "bathymetry", "speciesarea", "metabolism", 
		"sizespectrum", "speciescomposition", "temperature", "biochem", "condition" 
	) )

  p$libs = RLibrary( "mgcv", "sp", "gstat",  "parallel", "fields", "chron" ) 
  p$taxa = "maxresolved"
  p$season = "allseasons"
  p$interpolation.distances = c( 2, 4, 8, 16, 32, 64, 80 ) 
  p$interpolation.nmax = 100 
   
 

  p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far

  p$yearstomodel = 1970:2014
  
   
     p$speciesarea.modeltype = "complex"
     p$speciesarea.method = "glm"   ## this is chosen in speciesarea.r ... make sure it matches up
     p$speciesarea.season = "allseasons"  
     p$speciesarea.taxa = "maxresolved"  # use only unique taxa
     p$speciesarea.data.sources = c("groundfish", "snowcrab")
     p$speciesarea.variables = c( "C", "Z", "T", "Npred" )
     
     p$speciescomposition.modeltype = "complex"  
     p$speciescomposition.season = "allseasons"  
     p$speciescomposition.taxa = "maxresolved"  
     p$speciescomposition.variables = c( "ca1", "ca2" )
 
     p$sizespectrum.modeltype = "complex" 
     p$sizespectrum.taxa = "maxresolved"
     p$sizespectrum.season = "allseasons"
     p$sizespectrum.variables = c( "nss.b1", "nss.rsquared", "nss.shannon")
 
     p$condition.modeltype = "complex" 
     p$condition.taxa = "maxresolved"
     p$condition.season = "allseasons"
     p$condition.variables = c( "coAll", "coFish", "coElasmo", "coGadoid", "coDemersal", "coPelagic", 
                                "coSmallPelagic", "coLargePelagic", "coSmallDemersal", "coLargeDemersal")
 
     p$metabolism.modeltype = "complex" 
     p$metabolism.taxa = "alltaxa"
     p$metabolism.season = "allseasons"
     p$metabolism.variables = c( "smr", "Pr.Reaction" , "Ea", "A", "qn", "qm", "mass", "len"  )
 


  p$clusters = rep("localhost", detectCores() )
 # p$clusters = rep("localhost",8)
  # p$clusters = c( rep("kaos", 14), rep("tartarus", 14), rep("nyx", 14) )



