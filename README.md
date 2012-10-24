Various functions, scripts and tools useful for general ecological modelling, population stock assessments and habitat analysis. 

To download the respository, you can either:

  get the current snapshot of the whole respository (zip-compressed; see link near top), or 
	
  clone it using git (if on MSWindows, use Msysgit). 
  
The latter would allow you to contribute or merge updates with greater control. 
For help using git, see: http://git-scm.com/book/ .


To use these functions, you will need to add the following to the end of your Rprofile.site (Windows) or .Rprofile (Linux, MacOSX):

  ecomod.workdirectory = file.path( "C:", "R", "workspace" )    ### replace with correct path
  ecomod.directory = file.path( "C:", "path", "to", "ecomod" )  ### replace with correct path
	
  source( file.path( ecomod.directory, "default.rprofile.r" ) ) ### initializes the ecomod environment



To load in functions, you can either:

  loadfunctions( c("common", "snowcrab") )  

The above loads all functions under the common/src/_Rfunctions/ and snowcrab/src/_Rfunctions/   

or, 

  loadfunctions( "snowcrab", functionname="current.assessment.year.r")  

The above loads a specific function that fuzzy matches the functionname criterion under the directory snowcrab/src/_Rfunctions/ .



