# libraries
  RLibrary( "Hmisc", "date", "chron", "vegan", "fields" )

  gs.srcdir   = file.path( project.directory("groundfish"), "src" )
  data.dir  = file.path( project.directory("groundfish"), "data")
  gs.datadir  = file.path( project.directory("groundfish"), "data" )
  R.gs        = file.path( project.directory("groundfish"), "R" ) # main snowcrab project directory:

# helper functions  
init.files = loadfunctions( c( "plottingmethods", "spatialmethods", "utility", "parallel", "taxonomy", "temperature", "habitat", "bathymetry", "bio", "groundfish","BIOsurvey" ) )
  
  
setwd( R.gs )

