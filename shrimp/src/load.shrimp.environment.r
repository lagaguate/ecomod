  
  RLibrary( c( 
    "RODBC", "DBI", "Cairo", "Hmisc", "chron", "akima", "fields", "lattice", 
    "gstat", "rgdal", "maptools"
  ) )

  init.files = loadfunctions( c( "utility", "spacetime", "bathymetry", "shrimp") )
	
  
  # working directory for temporary/work files 
  workpath = file.path( project.datadirectory("shrimp"), "R" )
  dir.create( workpath, recursive=T, showWarnings=F )
  setwd (workpath)

  shrimp.data = file.path( project.datadirectory("shrimp"), "data" )
  dir.create( shrimp.data, recursive=T, showWarnings=F )


