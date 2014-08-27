
# libraries
  RLibrary( "Hmisc", "date", "chron", "vegan", "fields" )


# primary directories
  tmpdir            = tempdir()

# source directories
  srcdir            = project.directory("ecnasap", "src" )
  gs.srcdir         = project.directory("groundfish", "src" )
  ecnasapdir        = project.directory("ecnasap", "src" )
	
	loadfunctions( c("spatialmethods", "utility", "parallel", "taxonomy", "ecnasap", "speciesarea", "groundfish" )

