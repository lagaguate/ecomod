 
  tranf.db.lookup = function( db ) {

    if (db=="snowcrab") {
      log.transform = get.variables("log.transform")
      scaled.centered = get.variables("scaled.centered")
      sn = get.variables("all.data")
      set = snowcrab.db("set.complete") # base transform characteristics 
      repository = file.path( project.directory("snowcrab"), "R", "transform.lookup.rdata" )
    } else if (db=="groundfish") {
      log.transform = get.variables("log.transform")
      scaled.centered = get.variables("scaled.centered")
      sn = get.variables("all")
      set = groundfish.db("sm.complete" )    
      repository = file.path( project.directory("groundfish"), "R", "transform.lookup.rdata" )
    } else {
      print(" Must define data sources for transformation to this function" )
      stop()
    }
     return( list( log.transform=log.transform, sn=sn, set=set, repository=repository ) )
  }


