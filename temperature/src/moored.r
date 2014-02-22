
  init.files = c( 
    file.path( project.directory("temperature"), "src", "functions.moored.temperature.r" ),
    file.path( project.directory("common"), "src", "functions.date.r" ) ,
    file.path( project.directory("common"), "src", "functions.parallel.r" ) 
  )
  
  clusters = c( rep( "kaos", 16), rep("nyx", 16), rep("tartarus", 16) )
  parallel.run( clusters=clusters, n=length(yrs), moord.db, DS="redo.all", yrs=yrs, init.files=init.files )
 
