
# libraries
  loadlibraries( "Hmisc", "date", "chron", "vegan", "fields" )


# primary directories
  tmpdir            = tempdir()

# source directories
  srcdir            = file.path( project.directory("ecnasap"), "src" )
  gs.srcdir         = file.path( project.directory("groundfish"), "src" )
  ecnasapdir        = file.path( project.directory("ecnasap"), "src" )
  commondir         = file.path( project.directory("common" ), "src" )

	
	loadfunctions( c("common", "taxonomy", "ecnasap", "speciesarea", "groundfish" )

