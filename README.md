
Welcome.


This is a code repository of various functions, scripts and tools useful for general ecological modelling, population stock assessments and habitat analysis. The code is mostly implemented in the R-language (http://www.r-project.org/) and intended to be run through the free and open-source R-environment. There is some occasional reliance upon other languages such as python, JAGS/BUGS, GMT (Generic Mapping Tools), JAVA, Fortran, C, etc.

Some of these subprojects are inter-related in that they refer to each other's functions and data streams. Some of the subprojects are didactic, demonstrating approach or simply documenting the methods (especially as some data sets are not publicly available). However, all the subprojects are here to further collaboration, communication and transparency, to reduce the replication of effort and to permit other researchers to apply these methods and improve upon them. 

Rather than implementing the repository as 'R-packages', we use the GIT version control system as it permits rapid and distributed development of code. In other words, everything here is work-in-progress and subject to breakage and documentation will be in some ways lacking. You will have to follow the example scripts to help you understand usage and functionality. Nonethless, the Master branch visible here is as near to production-level code as possible. It serves as the operational basis of real research and applied science programs.

To download this repository, you can either:

  1. Get the current snapshot of the whole repository (zip-compressed; see link near top), or 
  2. Clone it using git (if on MSWindows, use Msysgit; for help see http://git-scm.com/book/; again the link is near the top of this page). The latter would allow you to contribute or merge your own work or modifications. 


To fully use this respository, you will need to add the following to the end of your Rprofile.site in MSWindows (usually found in the "etc" directory where install R) or ~/.Rprofile in Linux and other UNIX variants, including MacOSX:

    ecomod.workdirectory = file.path( "C:", "R", "workspace" )    ### replace with correct path
    ecomod.directory = file.path( "C:", "path", "to", "ecomod" )  ### replace with correct path
	
    source( file.path( ecomod.directory, "ecomod.rprofile.r" ) ) ### initializes the ecomod environment



Example usage: To load in all functions under the common/src/_Rfunctions/ and snowcrab/src/_Rfunctions/ you need to run the following:

    loadfunctions( c("common", "snowcrab") )  

or to load a specific function that fuzzy matches the 'functionname' criterion under the directory snowcrab/src/

    loadfunctions( "snowcrab", functionname="current.assessment.year.r")  


The function, 'loadfunctions()' is a convenience wrapper for reading in all files under a particular project directory. 

Best,
Jae

