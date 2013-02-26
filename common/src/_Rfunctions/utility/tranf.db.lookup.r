 
  tranf.db.lookup = function( db ) {

    if (db=="snowcrab") {
      log.transform = variable.list.expand("log.transform")
      scaled.centered = variable.list.expand("scaled.centered")
      sn = variable.list.expand("all.data")
      set = snowcrab.db("set.partial") # base transform characteristics 
      repository = file.path( project.directory("snowcrab"), "R", "transform.lookup.rdata" )
    } else if (db=="groundfish") {
      log.transform = variable.list.expand("log.transform")
      scaled.centered = variable.list.expand("scaled.centered")
      sn = variable.list.expand("all")
      set = groundfish.db("sm.partial" )    
      repository = file.path( project.directory("groundfish"), "R", "transform.lookup.rdata" )
    } else {
      print(" Must define data sources for transformation to this function" )
      stop()
    }
     return( list( log.transform=log.transform, sn=sn, set=set, repository=repository ) )
  }


