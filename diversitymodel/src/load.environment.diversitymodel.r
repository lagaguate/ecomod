
  env.init = c(
		file.path( project.directory("common"), "src", "functions.map.r" ),
		file.path( project.directory("common"), "src", "functions.spatial.r" ),
		file.path( project.directory("common"), "src", "functions.date.r" ),
		file.path( project.directory("common"), "src", "functions.filter.r" ),
    file.path( project.directory("common"), "src", "functions.parallel.r" ),
		file.path( project.directory("common"), "src", "functions.conversion.r" ),
		file.path( project.directory("common"), "src", "functions.utility.r" ),
		file.path( project.directory("common"), "src", "geodesy.r" ),
    file.path( project.directory("bathymetry"), "src", "functions.bathymetry.r" ),
    file.path( project.directory("temperature"), "src", "functions.temperature.r" ),
    file.path( project.directory("habitat"), "src", "functions.habitat.r" ),  # watch out: this accesses temperatures -- must be run before
    file.path( project.directory("taxonomy"), "src", "functions.taxonomy.r" ),
    file.path( project.directory("taxonomy"), "src", "functions.itis.r" ),
    file.path( project.directory("bayesian"), "src", "functions.jags.r" ), 
    file.path( project.directory("bio"), "src", "functions.bio.r" ),
    file.path( project.directory("speciesarea"), "src", "functions.speciesarea.r" ),
    file.path( project.directory("diversity")model, "src", "functions.diversitymodel.r" )
  )
  
  for (i in env.init) source (i)


  p = list()
  p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
  p$init.files = env.init
  p$pred.radius = 50 # km
  p$timescale = c( 0) # yr
  p$lengthscale = c( 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100, 110, 120 )  # km used in counting for rarefaction curve
  p$taxa = "maxresolved"
  p$taxa.secondary.filter = "allfish"
  p$season = "allseasons"
  p$data.sources = "groundfish"
  # p$interpolation.distances = 25  # habitat interpolation scale
  # p$speciesarea.method = "glm"  # dummy ... needed? 
  # p$yearstomodel = 1970:2011 # set map years separately to temporal.interpolation.redo allow control over specific years updated
 

    sar = speciesarea.db(DS="speciesarea.counts", p=p)
    sar = sar[,1,] # remove the time dimension
    sar = cbind( speciesarea.db(DS="speciesarea.set.intermediate", p=p ), sar )

		debug = T
		if (debug) {
			sar = sar[ which (sar$yr %in% c(2005:2010) ) ,]
			# sar = sar[ sample( 1000), ]
      # sar$yr = c( 1:10)
      # sar$idnum = as.numeric(as.factor(sar$idnum)) 
		}
    sar$yr = sar$yr - min(sar$yr) + 1
		
		
    sar$idnum = as.numeric( as.factor( sar$id ) )
    nums = as.character( 1:length(p$lengthscale))
    Robs = as.matrix( sar[ , nums] )
		Robs[ which(Robs==0) ] = NA 	
  
	
  sb = list( 
    nStations = max(sar$idnum) ,
    nYears = max(sar$yr), 
    nSizes = length(nums ),
    Stations = sar$idnum,
    Years = sar$yr,
    SA = pi * (p$lengthscale)^2 / 1000,
    SAsystem = 100,
    Robs = Robs,
    eps = 1e-4  # small non-zero number
  )
 
  gc()



