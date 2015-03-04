
	
  ### requires an update of databases entering into analysis: 
  # snow crab:  "cat" and "set.clean"
  # groundfish: "sm.base", "set"
  # and the glue function "bio.db"


# create base species area stats  ... a few hours

  p = list()
  p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
  p$init.files = loadfunctions( c("spacetime", "utility", "parallel", "bathymetry", "temperature", "habitat", "taxonomy", "bio", "speciesarea"  ) )

  p$data.sources = c("groundfish", "snowcrab") 
  p$speciesarea.method = "glm" 
  
  p$pred.radius = 50 # km
  p$lengthscale = c( 10, 15, 20, 25, 30, 40, 50, 60, 80, 100 )  # km
  p$timescale = c( 0, 1, 2 ) # yr
  
  p$taxa = "maxresolved"
  p$season = "allseasons"
 
  p$modeltype = "complex"   ## primary difference in models --- 


  kk = speciesarea.db( DS="speciesarea.stats", p=p ) # ~ 1 minute
  kk$decade = floor(kk$yr/10)*10
  kk$decade[ which( kk$decade==2010) ] = 2000

  hist( kk$C, "fd" )
  hist( kk$Z ,"fd")
  hist( kk$T ,"fd")
  hist( kk$Npred ,"fd")

  require(lattice)
  histogram( ~ C | decade, kk, breaks="fd"  )
  histogram( ~ T | decade, kk , breaks="fd")
  histogram( ~ Z | decade, kk , breaks="fd")
  histogram( ~ Npred | decade, kk , breaks="fd")


  
  mm =  speciesarea.db( DS="speciesarea.models", p=p )
 


  lapply( mm, summary ) 




