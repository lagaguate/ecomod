
  # example code to use the spacetime interpolation module
  # inputs: lon, lat, time, var as an array
  # internally converts to planar coords and then grids the data 
  
  p = list()
	p$init.files = loadfunctions( "spacetime" )
  p$libs = RLibrary( c( "chron", "lubridate", "parallel","sp" )  )
  p$clusters = rep("localhost", detectCores() )
 
  p = spatial.parameters( p, "SSE" )  # data  domain 
  p$crs = lookup.projection.params( p$internal.projection )

  p$project.name = "satellite.color"
  p$project.outdir.root = project.datadirectory( p$project.name, "analysis" )

  
  # 1. discretise to space, time  blocks or not ..
  
  # 2. interpolate in time with small spatial extent  .. constraint length autocor length (and n samp)
  
  # 3. interpolate in space with small time extent .. constraint temporal autocor lenth (or no. samp)
  # repeat until convergence
  #

