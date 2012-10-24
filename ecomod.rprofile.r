
	# ecomod.directory
	if (is.null( ecomod.directory )) {
		stop( "Please define 'ecomod.directory' in your Rprofile.site (Windows) or .Rprofile (Linux/MacOS" ) 
	}


	# ecomod.work.directory
	if (is.null( ecomod.work.directory )) {
		ecomod.work.directory = getwd()
	} else {
		setwd( ecomod.work.directory )
	}


	# name of the start-up file (this file) .. used for initializing parallel processing
	ecomod.rprofile  = file.path( ecomod.directory, "default.rprofile.r" )  
	

	# load bootstrapping functions to finish initial setup
	startupdirectory = file.path( ecomod.directory, "common", "src", "_Rfunctions", "startup" )
	startupfiles = list.files( path=startupdirectory )
	for ( sf in startupfiles ) source( sf )



	# base environment .. used to manipulate namespace 
  lib.init = search()
  obj.init = ls()
  namespaces.init = loadedNamespaces()


