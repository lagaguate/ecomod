 

  # ----------------------------------------------------------------------------------
  # NOTE to all: The year of "current.assessment.year must be changed every year before any other run
  #       It cannot be automatically loaded together with the "load.snowcrab.environment". This is because 
  #       running in parallel mode requires overrding some parameters in "p" on occasion which cannot be done cleanly 
  #       as "load.snowcrab.environment" is sourced with every initialisation of a  new CPU.
  #       Copying the following into each relevent file is not a solution as it is error prone and  repetitive. 
  # ----------------------------------------------------------------------------------

  loadfunctions( "snowcrab", functionname="current.assessment.year.r") 
  loadfunctions( "snowcrab", functionname="parameter.list.snowcrab.r") # load the function first  

  p = parameter.list.snowcrab ( current.assessment.year=current.assessment.year, set="default")
  
  workpath = file.path( project.datadirectory("snowcrab"), "R" )
  dir.create( workpath, recursive=T, showWarnings=FALSE )
  setwd (workpath)

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
  
# default time format
  dateformat.snow = c(dates="year-m-d", times="h:m:s")  # default date output format for chron objects
  

