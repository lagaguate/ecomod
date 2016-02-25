
  p = list( project.name = "coastline" )

  p$project.root = project.datadirectory( p$project.name )
         
  p$init.files = loadfunctions( c( "spacetime", "utility", "bathymetry", "coastline", "polygons" )  )
  p$libs = RLibrary( c( "rgdal", "maps", "mapdata", "maptools", "geosphere", "sp", "raster", "rgeos" ))
  
  # default (= only supported resolution of 0.5 km discretization)  .. do NOT change 
  # use "spde_complete" to project/downscale/upscale onto other grids/resolutions
  p = spatial.parameters( type="canada.east.highres", p=p ) 
  
  # worldHires coastlines from mapdata (still pretty crude ..)
  coast = coastline.db( p=p, DS="mapdata.coastLine.redo" ) # flatten into one
  coast = coastline.db( p=p, DS="mapdata.coastPolygon.redo" )
 

  # GSHHG coastline data (NOAA) -- best quality data
  # download the current version of the GSHHG coastline data 
  #  coastline.db( "gshhg.download" )  << ---- RUN THIS if you want to refresh the local copy from NOAA
  
  # these are generated on the fly the first pass .. p must contain corner$lon/lat
  # and ensure they are the largest domain for your use (here "canada.east.highres" )
  p = spatial.parameters( type="canada.east.highres", p=p ) 
  
  u = coastline.db ( p=p, DS="gshhg coastline full redo", no.clip=TRUE  ) # full is all data
  u = coastline.db ( p=p, DS="gshhg coastline highres redo ", no.clip=TRUE  ) # highres is almost all data
  u = coastline.db ( p=p, DS="gshhg coastline intermediate redo", no.clip=TRUE  ) # medium res
  u = coastline.db ( p=p, DS="gshhg coastline low redo ", no.clip=TRUE  ) # low res
  u = coastline.db ( p=p, DS="gshhg coastline crude redo ", no.clip=TRUE  ) # crude is very rough 

  # note you can also get rivers and political boundaries .. these are spatial Lines
  u = coastline.db ( p=p, DS="gshhg rivers full redo " ) # crude is very rough 
  u = coastline.db ( p=p, DS="gshhg rivers highres redo " ) # crude is very rough 
  u = coastline.db ( p=p, DS="gshhg rivers intermediate redo " ) # crude is very rough 
  
  u = coastline.db ( p=p, DS="gshhg borders full redo " ) # crude is very rough 
  u = coastline.db ( p=p, DS="gshhg borders highres redo " ) # crude is very rough 
  u = coastline.db ( p=p, DS="gshhg borders intermediate redo " ) # crude is very rough 

  # to call without p$corners:
  u = coastline.db ( DS="gshhg coastline full redo", xlim=p$corners$lon, ylim=p$corners$lat ) # full is all data
 

  #example plot 
  depths = c( 100, 200, 250, 300, 400, 500)
  plygn = isobath.db( p=p, DS="isobath", depths=depths  )
  
  coast = coastline.db( p=p, DS=" gshhg coastline highres", no.clip=TRUE )
  plot( coast, col="transparent", border="steelblue2" , 
       xlim=c(-68,-52), ylim=c(41,50),  xaxs="i", yaxs="i", axes=TRUE )  # ie. coastline
  lines( plygn[ as.character(depths)], col="gray90" ) # for multiple polygons

  sp::compassRose( -55, 43, cex= 0.75 )
  maps::map.scale( -57, 42, ratio =FALSE )

