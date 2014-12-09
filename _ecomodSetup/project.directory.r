

project.directory = function( ... ) {
  
  ## this function is required to bootstrap the other project level functions
  ## probably should make this cleaner one day .. :)

  # determine project directory string
  sep = .Platform$file.sep
  dirs = paste0( c(...) , collapse=sep ) 
  pd = file.path( ecomod.directory, dirs )
  pd = gsub( paste( sep, "$", sep=""), "", pd) # strip last path element 
  
  # nonexistant directories 
  if (! file.exists( pd)) {
    mk.Dir = readline("Directory not found, shall we create it? Yes/No (Default No) : ")
    if ( mk.Dir =="Yes" ) {
      dir.create( pd, recursive=TRUE) 
      print( paste("The directory -- ",  pd, " -- has been created") )
    } else {
      warning( paste("Directory: ", pd, " was not found." )) 
    }
  }

  return ( pd )
}


