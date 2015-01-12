
parameter.list.snowcrab = function ( p=list(), current.assessment.year, set="default" ) {
   
  if ( set =="default" ) {
    
    p$current.assessment.year = current.assessment.year 

    p$libs = RLibrary ( c( 
     "lubridate", "mgcv", "parallel", "DBI", "Cairo", "Hmisc", "chron", "vegan", "akima", "fields", "lattice", "gstat", "rgdal", "maptools",  "boot"
    ) )

    p$init.files = loadfunctions( c( "spatialmethods", "utility", "parallel", "polygons", "snowcrab", "groundfish", "substrate", "temperature", "taxonomy", "habitat", "habitatsuitability", "bathymetry", "plottingmethods" ) )

    p$annual.results = file.path( project.directory("snowcrab"), "assessments", current.assessment.year ) # output location for year-specific results
    
    p$spatial.domain = "snowcrab"
    p = spatial.parameters( p ) # region and lon/lats, projections 
    p = gmt.parameters( p ) 


    p$annot.cex=2


    # ---------------------------------------------------
    # clusters definition
        p$do.parallel = T
   #    p$clusters = c( rep("io",8), rep("tethys",2) )  # put the fast cpu's first
   #    p$clusters = c( rep("localhost",8) )  # use local cluster to make file usage easier without refreshing each filesystem contantly
        p$clusters = c( rep("tethys",8) )  # use local cluster to make file usage easier without refreshing each filesystem contantly

      p$fisheries.grid.resolution = 2 

      p$ofname = file.path(p$annual.results, paste("TSresults", p$current.assessment.year, "rdata", sep=".") )
      
      p$regions.to.model = c( "cfanorth", "cfasouth", "cfa4x", "cfaall" )
      p$vars.to.model = variable.list.expand("all.to.model")
      p$years.to.model = c(1998:p$current.assessment.year)
      
      p$yearswithTdata = c(1950:p$current.assessment.year)  
      p$recode.data = T
      p$map.results=T
      
      
      # p$kformula = as.formula( "kv ~ z + t + total.cpue" )  # model in 2006-2008
      # p$kformula = as.formula( "kv ~ z + t + tamp + wmin + dZ + ddZ + substrate.mean + total.cpue" )  # model in 2006-2008
      p$kformula = as.formula( "kv ~ z + t + tamp + wmin + dZ + ddZ + substrate.mean" )  # model in 2006-2008
#    p$kformula = as.formula( "kv ~ z + t + tamp + wmin + dZ + ddZ + substrate.mean " )  # model in 2006-2008
      p$klocs = as.formula ( "~plon+plat" )
      p$vgm.dist = unique(sort(c( seq(10, 60, 4), seq(50, 100, 10), seq( 80, 160, 20) )))
      p$knmax=100  # must be greater than 30 for convergence
      p$krige.incremental = F
      p$plot.variogram = F
      p$transgaussian.kriging = T  
      p$n.conditional.sims = 100
      # p$inflection.t = 2
      # p$inflection.z = 250
      # p$linearise.drift.terms = T
      p$threshold.distance = 5  # in km for merging fisheries data into the trawl data for external drift kriging
     

      # default plotting time format
      p$plottimes=c("annual", "globalaverage")

      # default figure generation (from maps)
      p$conversions=c("ps2png")
    }

  return (p)
}
