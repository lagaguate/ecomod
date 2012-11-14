
# libraries
  loadlibraries( "Hmisc", "date", "chron", "vegan", "fields" )


# primary directories
  tmpdir            = tempdir()

# source directories
  srcdir            = project.directory("ecnasap", "src" )
  gs.srcdir         = project.directory("groundfish", "src" )
  ecnasapdir        = project.directory("ecnasap", "src" )
  commondir         = project.directory("common" , "src" )

	
	loadfunctions( c("common", "taxonomy", "ecnasap", "speciesarea", "groundfish" )

