
# libraries
  RLibrary( "Hmisc", "date", "chron", "vegan", "fields" )


# primary directories
  tmpdir            = tempdir()

# source directories
  srcdir            = project.codedirectory("ecnasap", "src" )
  gs.srcdir         = project.codedirectory("groundfish", "src" )
  ecnasapdir        = project.codedirectory("ecnasap", "src" )
	
	loadfunctions( c("spacetime", "utility", "parallel", "taxonomy", "ecnasap", "speciesarea", "groundfish" )

