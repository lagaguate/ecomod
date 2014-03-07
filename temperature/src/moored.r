  p=list()
  p$init.files = c( 
    file.path( project.directory("temperature"), "src", "functions.moored.temperature.r" ),
    file.path( project.directory("common"), "src", "functions.date.r" ) ,
    file.path( project.directory("common"), "src", "functions.parallel.r" ) 
  )
  p$clusters = c( rep( "kaos", 16), rep("nyx", 16), rep("tartarus", 16) )
  p = make.list( list( yrs=yrs), Y=p )
  parallel.run( moord.db, DS="redo.all" )
 


