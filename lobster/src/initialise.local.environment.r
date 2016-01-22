   

   p = list()
   p$init.files = c(p$init.files,loadfunctions(c("BIOsurvey",'polygons','lobster','groundfish','redfish','utility','parallel') ))
    p$libs = RLibrary( c( "chron", "lubridate", "parallel","sp","PBSmapping","RColorBrewer" ,'lattice','MASS','doBy','RODBC')  )
      
      p$current.assessment.year=2016
  

# Global lobster parameters
