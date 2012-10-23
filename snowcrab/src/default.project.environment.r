

  loadlibraries ( c( 
    "DBI", "RSQLite", "Cairo", "Hmisc", "chron", "vegan", "akima", "fields", "lattice", 
    "gstat", "rgdal", "maptools"
  ) )


	# files required to initialised the same base state when running in parallel mode
  init.files = loadfunctions( c("common", "snowcrab", "groundfish", "substrate", "temperature", "taxonomy", "habitat" ) )
	init.files = c( ecomod.rprofile, init.files )
 
	workpath = file.path( project.directory("snowcrab"), "R" )
  dir.create( workpath, recursive=T, showWarnings=F )
  setwd (workpath)


  # snow crab related data bases and tables
  db.snow =  file.path( project.directory("snowcrab"), "R" , "snowcrab.sqlite.db" )
  setInitial = "setInitial"
  setMinilogLookup = "setMinilogLookup"
  setNetmindLookup = "setNetmindLookup"
  setLookup= "setLookup" # netmind and minilog lookup data
  setClean= "setClean" 


  minilog.rawdata.location = file.path( project.directory("snowcrab"), "data", "minilog", "archive" )
  mDB = file.path( project.directory("snowcrab"), "data", "minilog", "minilog.sqlite.db" )
  mMeta = "minilog_metadata"
  mBase = "minilog_basedata"
  mStats = "minilog_bottom_stats"


  seabird.rawdata.location = file.path( project.directory("snowcrab"), "data", "seabird", "archive" )
  sDB = file.path( project.directory("snowcrab"), "data", "seabird", "seabird.sqlite.db" )
  sMeta = "seabird_metadata"
  sBase = "seabird_basedata"
  sStats = "seabird_bottom_stats"



  # global vars internal to netmind world 
  netmind.rawdata.location = file.path( project.directory("snowcrab"), "data", "netmind", "archive" )
  nDB = file.path( project.directory("snowcrab"), "data", "netmind", "netmind.sqlite.db" )
  nMeta = "netmind_metadata"
  nBase = "netmind_basedata"
  nStats = "netmind_stats"



  # Global snow crab parameters
  
	# sex codes
    male = 0 
    female = 1
    sex.unknown = 2

  # maturity codes
    immature = 0
    mature = 1 
    mat.unknown = 2

    planar.corners = data.frame(rbind( cbind( plon=c(220, 990), plat=c(4750, 5270) ))) # for plots in planar coords
    
  # default plotting time format
    plottimes=c("annual", "globalaverage")

  # default figure generation (from maps)
    conversions=c("ps2png")
 
	# default time format
    dateformat.snow = c(dates="year-m-d", times="h:m:s")  # default date output format for chron objects
 

