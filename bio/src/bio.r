
  # glue biological data sets together from various surveys

	p = list( init.files=loadfunctions( c( "common", "taxonomy", "groundfish", "snowcrab", "bio" ))) 

  loadfunctions( "snowcrab", functionname="default.project.environment" )

	p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
  p$taxa =  "maxresolved"
  # p$seasons = "allseasons"
	p$data.sources = c("groundfish", "snowcrab") 

  bio.db( DS="set.redo", p=p )
  bio.db( DS="cat.redo", p=p )
  bio.db( DS="det.redo", p=p )
 
	bio.db( DS="cat.fixed.redo", p=p ) # a catch db where quantiles are computed  TODO



