 
  lookup.datatransformation = function( db ) {
    
    # determine data transformations based upon category of data and data source
    
    if (db=="snowcrab") {
      log.transform = variable.list.expand("log.transform")
      scaled.centered = variable.list.expand("scaled.centered")
      sn = variable.list.expand("all.data")
      set = snowcrab.db("set.merge.cat") # base transform characteristics 
      logs = logbook.db('logbook')
      repository = file.path( project.directory("snowcrab"), "R", "transform.lookup.rdata" )
    } else if (db=="groundfish") {
      log.transform = variable.list.expand("log.transform")
      scaled.centered = variable.list.expand("scaled.centered")
      sn = variable.list.expand("all")
      set = groundfish.db("set.partial" )    
      repository = file.path( project.directory("groundfish"), "R", "transform.lookup.rdata" )
    } else {
      print(" Must define data sources for transformation to this function" )
      stop()
    }
     return( list( log.transform=log.transform, sn=sn, set=set,logs = logs, repository=repository ) )
  }


