
	### requires an update of databases entering into analysis: 
  # snow crab:  "cat" and "set.clean"
  # groundfish: "sm.base", "set"
  # and the glue function "bio.db"

# create base species area stats  ... a few hours
  p = list( project.name = "speciesarea" )
  
  p$libs = RLibrary ( c( 
      "lubridate", "chron", "bigmemory", "mgcv", "sp", "parallel", "grid" , "lattice", 
      "fields", "rgdal", "raster" )) 

  p$init.files = loadfunctions( c( 
    "spacetime", "utility", "parallel", "bathymetry", "temperature", "habitat",
    "taxonomy", "bio", "speciesarea"  ) )
 
  p$year.focal = 2015 
  p$yearstomodel = 1970:p$year.focal # set map years separately to temporal.interpolation.redo allow control over specific years updated

  p = spatial.parameters( p, "SSE" )  # data are from this domain .. so far
  p = speciesarea.parameters(p) # load default parameters  

  p0 = p # save copy in case things get over-written
  
  # choose:
  # n  = 2
  # n = detectCores()  # this is the default
  # p$clusters = rep( "localhost", n )
  # p$clusters = rep(c("kaos", "nyx", "tartarus"), n)
 
# -------------------------------------------------------------------------------------
# Run BIO.DB to update the multi-survey databases /home/jae/ecomod/bio/src/bio.r
# -------------------------------------------------------------------------------------

  # count and record rarification curves from all available data --- refresh "bio.db" ~/ecomod/bio/src/bio.r  
  speciesarea.db( DS="speciesarea.counts.redo", p=p )  # 60 MB / process  -- can use all cpus

  # compute species-area relationships 
  speciesarea.db( DS="speciesarea.stats.redo", p=p ) # ~ 1 minute
  speciesarea.db( DS="speciesarea.redo", p=p ) # intermediary file for modelling and interpolation ... lookup up missing data and covariates


# -------------------------------------------------------------------------------------
# Generic spatio-temporal interpolations and maping of data 
# using the interpolating functions and models defined in ~ecomod/habitat/src/
# -------------------------------------------------------------------------------------
  
  p$clusters = rep("localhost", length( p$varstomodel ) ) 
  p = make.list( list(vars= p$varstomodel ), Y=p ) 
  parallel.run( habitat.model, DS="redo", p=p ) 
  # habitat.model ( DS="redo", p=p ) 

  # predictive interpolation to full domain (iteratively expanding spatial extent)
  p$clusters = rep("localhost", 10) # 6 GB / process
  p = make.list( list(vars= p$varstomodel, yrs=p$yearstomodel ), Y=p ) 
  parallel.run( habitat.interpolate, p=p, DS="redo" ) 
  #  habitat.interpolate( p=p, DS="redo" ) 

  # map everything
  p$clusters = rep("localhost", detectCores() )
  p = make.list( list(vars=p$varstomodel, yrs=p$yearstomodel ), Y=p )
  parallel.run( habitat.map, p=p  ) 
  # habitat.map( p=p  ) 




