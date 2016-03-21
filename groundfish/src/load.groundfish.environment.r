# libraries
  RLibrary( "Hmisc", "date", "chron", "vegan", "fields", "polygons", "coastline", "sp", "rgdal", "raster" )

  gs.srcdir   = file.path( project.codedirectory("groundfish"), "src" )
  data.dir  = file.path( project.datadirectory("groundfish"), "data")
  gs.datadir  = file.path( project.datadirectory("groundfish"), "data" )
  R.gs        = file.path( project.datadirectory("groundfish"), "R" )
  if (!file.exists(R.gs)) dir.create(R.gs)
# helper functions  
init.files = loadfunctions( c( "plottingmethods", "spacetime", "utility", "parallel", "taxonomy", 
  "netmensuration", "temperature", "habitat", "bathymetry", "bio", "groundfish", "polygons", "coastline" ))#,"BIOsurvey" ) )
  
setwd( R.gs )

