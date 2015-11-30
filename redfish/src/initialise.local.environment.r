   p = list()
   p$init.files = c(p$init.files,loadfunctions(c("BIOsurvey",'polygons','lobster','groundfish','redfish','utility','parallel') ))
    p$libs = RLibrary( c( "chron", "lubridate", "parallel","sp","PBSmapping","RColorBrewer" )  )
      p$strat=c(456,458:495)  #unit III - subtract 457 for unit 3
      p$series =c('summer')# p$series =c('4vswcod');p$series =c('georges')
      p$years.to.estimate = c(1970:2015)
      p$species = 23
      p$vessel.correction = T
      p$vessel.correction.fixed = 1.2
       p$functional.groups = F
      p$strata.efficiencies = F   

  workpath = file.path( project.datadirectory("redfish"), "R" )
  dir.create( workpath, recursive=T, showWarnings=FALSE )
  setwd (workpath)

# Global snow crab parameters
