

project.directory = function( name,  ... ) {
  
  ## this function is required to bootstrap the other project level functions
  ## probably should make this cleaner one day .. :)

  # determine project directory string
  pd = file.path( ecomod.directory, name, paste0(c(...), collapse=.Platform$file.sep ) )
  pd = gsub( paste(.Platform$file.sep, "$", sep=""), "", pd) # strip last path element 
  
  # filter out nonexistant directories
  pd = pd[ which( file.exists( pd) ) ] 
  
  return ( pd )
}


