
  # example code to use the spacetime interpolation module
  # inputs: lon, lat, time, var as an array
  # internally converts to planar coords and then grids the data 
  
  p = list()
	p$init.files = loadfunctions( c( "spacetime", "temperature" ) )
  p$libs = RLibrary( c( "lubridate", "parallel","sp" )  )
  p$clusters = rep("localhost", detectCores() )
 
  # p$project.name = "satellite.color"
  p$project.name = "temperature"
  
  p$project.outdir.root = project.datadirectory( p$project.name, "analysis" )

  p = spatial.parameters( p, "SSE" )  # data  domain 
  p$crs = lookup.projection.params( p$internal.projection )

  p$vars.required = c("timestamp", "longitude", "latitude" )
  p$vars.focal = "temperature"
  p$vars.covar = c( "depth", "salinity", "sigmat", "oxyml" ) 
  p$vars.keep = c( p$vars.required, p$vars.focal, p$vars.covar ) 

  # 1. discretise to space, time  blocks or not ..
  p$ti.offset = 0  # to do a phase shift of time 
  p$ti.scale = 1/4  # fraction of year to discretize: (i.e., 1 / no time increments in a year ) 
  p$sp.scale = 10   # spatal scale to discretize: km 
  p$edge = c(1/3, 1)
  
  p$tyears = c(1950:2014)
  
  U = NULL
  for( yr in p$tyears ) {
    Z = hydro.db( DS="bottom.annual", p=p, yr=yr )
    Z$timestamp = ymd_hms( paste (Z$date, "12:00:00" ) )  
    Z = Z[, p$vars.keep]
    U = rbind( U, Z )
  }
  rm (Z)

  uu = which( names(U) == "longitude"); names(U)[uu] = "lon"
  uu = which( names(U) == "latitude");  names(U)[uu] = "lat"
  
  U = lonlat2planar( U, proj.type=p$crs )
  U$lon = U$lat = NULL 

  # 2. interpolate in time with small spatial extent  .. constraint length autocor length (and n samp)
  
  # 3. interpolate in space with small time extent .. constraint temporal autocor lenth (or no. samp)
  # repeat until convergence
  
  # discretize time
  U$ts = discretize.time( U$timestamp, ti.scale=p$ti.scale )
  U$timestamp = NULL

	
  U$plon = grid.internal( U$plon, p$plons )
  U$plat = grid.internal( U$plat, p$plats )
	
  
  drange = sqrt(diff(range(U$plon))^2 + diff(range(U$plat))^2)
  
  xrange = range( p$plons, na.rm=TRUE )
  yrange = range( p$plats, na.rm=TRUE )
  zrange = range( U[, p$vars.focal], na.rm=TRUE )
  
  difx = diff( xrange) 
  dify = diff( yrange) 

  nn = 400
  nxout = trunc(nn * difx / dify)
  nyout = nn
  nzout = 100



