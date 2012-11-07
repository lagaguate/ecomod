
  # glue biological data sets together from various surveys

  require(RSQLite)

	p = list( init.files = loadfunctions ( c( 
		"common", "taxonomy", "groundfish", "snowcrab", "bio" 
	) ) ) 

	p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
  p$taxa =  "maxresolved"
  # p$seasons = "allseasons"
	p$data.sources = c("groundfish", "snowcrab") 

  bio.db( DS="set.redo", p=p )
  bio.db( DS="cat.redo", p=p )
  bio.db( DS="det.redo", p=p )
 
	bio.db( DS="cat.fixed.redo", p=p ) # a catch db where zero's are explicit and error checked


