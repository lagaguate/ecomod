# ecomod.directory
if (is.null( ecomod.directory )) {
	stop( "Please define 'ecomod.directory' in your Rprofile.site (Windows) or .Rprofile (Linux/MacOS" ) 
}


# ecomod.workdirectory
if (is.null( ecomod.workdirectory )) {
	ecomod.workdirectory = getwd()
} else {
	setwd( ecomod.workdirectory )
}


# name of the start-up file (this file) .. used for initializing parallel processing
ecomod.rprofile  = file.path( ecomod.directory, "ecomod.rprofile.r" )  
	

# load bootstrapping functions to finish initial setup
startupdirectory = file.path( ecomod.directory, "common", "src", "_Rfunctions", "startup" )
startupfiles = list.files( path=startupdirectory, full.names=T, recursive=T,  ignore.case=T, include.dirs=F  )
for ( sf in startupfiles ) source( sf )



<<<<<<< HEAD
# base environment .. used to manipulate namespace -- to remove??
lib.init = search()
obj.init = ls()
namespaces.init = loadedNamespaces()
=======
	# base environment .. used to manipulate namespace 
  lib.init = search()
  obj.init = ls()
  namespaces.init = loadedNamespaces()
>>>>>>> develop


