
	# main options 
  
  options (width=120)
  options (browser = "dillo")
  options (papersize = "letter")
  options (editor = "gvim")
  options (digits = 6 )
  options (scipen=4)


# primary project directories
  projects  = file.path( homedir, "projects" )

	
### project list
##	project.list = c(
##		"bayesian", 
##		"bathymetry", 
##		"biochem", 
##		"bio", 
##		"common", 
##		"competition", 
##		"condition", 
##		"diversity", 
##		"diversitymodel", 
##		"ecnasap", 
##		"googleearth", 
##		"groundfish", 
##		"habitat", 
##		"habitatsuitability",
##		"indicators", 
##		"lobster", 
##		"metabolism", 
##		"polygons", 
##		"ser", 
##		"sizespectrum", 
##		"sorted.ordination", 
##		"shrimp", 
##		"snowcrab", 
##		"speciesarea", 
##		"speciescomposition", 
##		"stomachs", 
##		"substrate", 
##		"temperature", 
##		"taxonomy"
##	)
##
##	# create project directory names
##	for ( i in project.list ) assign( paste("project",i, sep="." ), file.path( projects, i ) )  
##
  
  oracle.taxonomy.server = "bank.canso3"
  oracle.groundfish.server = "bank.canso3"
  
  oracle.shrimp.server = "bank.canso3"
  oracle.shrimp.user = "Shrimp"
  oracle.shrimp.password = "k3m2byf"

  oracle.snowcrab.server = "bank.canso3"
  oracle.snowcrab.user = "snowcrab"
  oracle.snowcrab.password = "0507wel"

  oracle.personal.user = "choij"
  oracle.personal.password = ".kropotkin."
   
   
	# To update/install useful packages
  #	source( file.path( project.directory("common"), "functions.packages.r") )
  # package.install ()


