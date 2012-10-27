	project.directory = function( name, ... ) {
		# determine project directory string
    return ( file.path( ecomod.directory, name, paste0(c(...), collapse=.Platform$file.sep )  ) )
	}


