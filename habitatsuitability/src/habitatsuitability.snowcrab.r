
  # Habitat suitability estimation template 
   
  ### define parameter list to determine the correct category of taxa/size from "bio.db" 


	loadlibraries (c("chron", "fields", "rgdal", "snow", "mgcv", "arm" ))


	p = list()
  p$env.init = loadfunctions(c( "common", "taxonomy", "bio", "habitat", "temperature",  "bathymetry"	)) 
  
  p = spatial.parameters( type="SSE" ) # 4VWX
  p$studyarea = c( "4vwx" )
  # p$studyarea = c("4vwx", "5yz" )
  
  # set map years separately to temporal.interpolation.redo allow control over specific years updated
  p$yearstomodel = 1970:2011 
  # p$seasons = "allseasons"
  
  p$taxa =  "maxresolved"
  # p$taxa.secondary.filter = "" 

  # --------------
  # do spatial predictions using GAM

  debug = TRUE;  # debug = FALSE
  p$nsims = 1000;  if (debug) p$nsims = 100
  p$habitat.threshold.quantile = 0.05 # quantile at which to consider zero-valued abundance
  p$prediction.month = 7  # July
  
  p$optimizers = c( "nlm", "perf" )  # used by GAM

  # modeltype choice
  p$modeltype="simple"
  p$gam.model.pa = habitat.model.lookup (p$modeltype)

  p$subset = "snowcrab.female.large"

  # DATA source
	p$data.sources = c("groundfish", "snowcrab") 
	p$fn.bio.subset = file.path( project.directory("habitatsuitability"), "data", "SSE", paste(p$subset, "rdata", sep="." ) )

  
  p$subsetfunction = list( "which(cat=="")" , "which(det=="") " )    

  ##  UPDATE BIO data if not already up to date
  update.bio = FALSE
  if (update.bio) loadfunctions( "bio", functionname="bio.r" )

  # must generate or regenerate the correct data selection
  update.local.subset = FALSE
  if (update.local.subset) habitatsuitability.db( DS="bio.subset.redo", p=p ) 

  # example extraction  x = bio.db( DS="subset", p=p ) 


