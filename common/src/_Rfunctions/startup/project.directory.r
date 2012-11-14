	project.directory = function( name, ... ) {
		# determine project directory string
    pd = file.path( ecomod.directory, name, paste0(c(...), collapse=.Platform$file.sep ) )
    pd = gsub( paste(.Platform$file.sep, "$", sep=""), "", pd) # strip last path element 
    return ( pd )
	}


