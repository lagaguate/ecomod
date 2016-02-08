
  # glue biological data sets together from various surveys
 
  p = list( project.name = "bio" )
  p$project.root = project.datadirectory( p$project.name )
  
  p$libs = RLibrary( "mgcv", "sp", "gstat",  "parallel", "fields", "chron", "lubridate", "raster", "rgdal"  ) 
  p$init.files = unique( c (
    loadfunctions( c("spacetime", "utility", "parallel", "taxonomy", "bathymetry","temperature" )), 
    loadfunctions( c("groundfish", "habitat", "bio" )),
    loadfunctions( "snowcrab", functionname="default.project.environment" ) ))

  p$nw = 10 # number of intervals in time within a year in the temperature interpolations ( must match temperature.r 's value )
  

  p$interpolation.distances = c( 2, 4, 8, 16, 32, 64 ) # pseudo-log-scale
  p$default.spatial.domain = "canada.east"  # for temperature lookups
	p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
  p$taxa =  "maxresolved"
  # p$seasons = "allseasons"
	p$data.sources = c("groundfish", "snowcrab") 
  p$clusters = rep("localhost", detectCores() )
 

  # load and glue data together
  bio.db( DS="set.init.redo", p=p )
  print("Finished   bio.db( DS=set.init.redo, p=p )")
  bio.db( DS="cat.init.redo", p=p )
  print("Finished   bio.db( DS=cat.init.redo, p=p )")
  bio.db( DS="det.init.redo", p=p )
  print("Finished bio.db( DS=det.init.redo, p=p )")


  # sanity checking and creation of new variables
  bio.db( DS="set.intermediate.redo", p=p ) # adds temperature required for metabolism lookup in "det.redo"
  print("Finished bio.db (DS=set.intermediate.redo)")
  bio.db( DS="det.redo", p=p ) # mass/length imputation and sanity checking
  print ("Finished bio.db (DS=det.redo)")
  bio.db( DS="cat.redo", p=p ) # sanity checking and fixing mass estimates from det etc ... 
  print ("Finished bio.db(DS=cat.redo)")
  bio.db( DS="set.redo", p=p ) # mass/length imputation and sanity checking
  print ("Finished bio.db (DS=set.redo)")
  

    #  --- look in metabolism functions and complexity/condition

    # to obtain stats from l-w relationships used to impute mass/leng and estimate condition
    # a = length.weight.regression ( DS="parameters", p=p )
  
    # to obtain biomass estimates after correction for tow, etc.
    # a = biomass.estimation (DS="saved"", p=p )

   



