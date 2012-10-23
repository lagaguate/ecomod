  
  loadlibraries( c( 
    "RODBC", "DBI", "Cairo", "Hmisc", "chron", "akima", "fields", "lattice", 
    "gstat", "rgdal", "maptools"
  ) )

  localfiles = loadfunctions( c("common", "bathymetry", "shrimp") )
	
  init.files = c( ecomod.rprofile, localfiles )  

  
  # working directory for temporary/work files 
  workpath = file.path( project.directory("shrimp"), "R" )
  dir.create( workpath, recursive=T, showWarnings=F )
  setwd (workpath)

  shrimp.data = file.path( project.directory("shrimp"), "data" )
  dir.create( shrimp.data, recursive=T, showWarnings=F )


