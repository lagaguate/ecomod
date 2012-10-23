

	# ecomod.directory
	if (is.null( ecomod.directory )) {
		stop( "Please define 'ecomod.directory' in your Rprofile.site (Windows) or .Rprofile (Linux/MacOS" ) 
	}


	# load in a few key environment variables, convenience functions and alter a few functions 
	source( file.path( ecomod.directory, "default.environment.r" ) )
	source( file.path( ecomod.directory, "default.functions.r" ) )
	source( file.path( ecomod.directory, "default.libraries.r" ) )
		
	
	


