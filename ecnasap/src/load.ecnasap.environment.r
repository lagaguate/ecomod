
# libraries
  loadlibraries( "Hmisc", "date", "chron", "vegan", "fields" )


# primary directories
  tmpdir            = "/tmp"
  homedir           = "/home/jae"
  srcdir            = "/home/jae/src" 

# primary directories
#  tmpdir            = "/tmp"
#  homedir           = "/home/jae"
# above are declared in ~/.Rprofile 


# source directories
  srcdir            = file.path( homedir, "src" )
  projects          = file.path( homedir, "projects" )
  gs.srcdir         = file.path( project.directory("groundfish"), "src" )
  ecnasapdir        = file.path( project.directory("ecnasap"), "src" )
  commondir         = file.path( project.directory("common" ), "src" )

	
	loadfunctions( c("common", "taxonomy", "ecnasap", "speciesarea", "groundfish" )

