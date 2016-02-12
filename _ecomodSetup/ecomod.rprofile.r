
# ecomod.directory
if(!exists("ecomod.directory")){
	stop( "Please define 'ecomod.directory' in your Rprofile.site (Windows) or .Rprofile (Linux/MacOS" ) 
}

# ecomod.workdirectory
if(!exists("ecomod.workdirectory")){
	ecomod.workdirectory = getwd()
} else {
	dir.create( ecomod.workdirectory, showWarnings = FALSE, recursive = TRUE )
  setwd( ecomod.workdirectory )
}
	
# load bootstrapping functions to finish initial setup
ecomod.startupfiles = list.files( path=file.path( ecomod.directory, "_ecomodSetup" ), full.names=T, recursive=T,  ignore.case=T, include.dirs=F  )

# to prevent loop / race condition
iprof = grep( "ecomod.rprofile.r", ecomod.startupfiles )

for ( sf in ecomod.startupfiles[-iprof] ) source( sf )

ecomod.startupfiles  ## this is a global variable

