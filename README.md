

Welcome.


This is a code repository of various functions, scripts and tools useful for general ecological modelling, population stock assessments and habitat analysis. The code is mostly implemented in the R-language (http://www.r-project.org/) and intended to be run through the free and open-source R-environment. There is some occasional reliance upon other languages such as python, JAGS/BUGS, gmt, JAVA, fortran, C, etc.

Much of the code is inter-related in that the subprojects refer to each other. Some of the subprojects are didactic, demonstrating approach or simply documenting the methods (especially as some data sets are not publicly available). However, all the subprojects are here to further collaboration and communication and reduce replication of effort and permit other researchers to apply these methods and improve upon them. 

Rather than implementing the repository as 'R-packages' (which are also excellent ways of distributing code), we have opted to use the GIT version control mechanism as it permits rapid and distributed development of code. In other words, everything here is work-in-progress and subject to breakage and documentation will be in some ways lacking. You will have to follow the example scripts to help you understand usage and functionality.

To download this repository, you can either:
    Get the current snapshot of the whole repository (zip-compressed; see link near top), or 
    Clone it using git (if on MSWindows, use Msysgit; for help see http://git-scm.com/book/). 

The latter would allow you to contribute or merge updates. 


To fully use this respository, you will need to add the following to the end of your Rprofile.site (Windows; usually found in the "etc" directory where install R) or ~/.Rprofile (Linux, MacOSX):

    ecomod.workdirectory = file.path( "C:", "R", "workspace" )    ### replace with correct path
    ecomod.directory = file.path( "C:", "path", "to", "ecomod" )  ### replace with correct path
	
    source( file.path( ecomod.directory, "ecomod.rprofile.r" ) ) ### initializes the ecomod environment



Example usage: To load in all functions under the common/src/_Rfunctions/ and snowcrab/src/_Rfunctions/ you need to run the following:

    loadfunctions( c("common", "snowcrab") )  

or to load a specific function that fuzzy matches the 'functionname' criterion under the directory snowcrab/src/_Rfunctions/

    loadfunctions( "snowcrab", functionname="current.assessment.year.r")  


The function, 'loadfunctions()' is simply a convenience wrapper for reading in all files under a particular project directory. 

More later.

Enjoy,
Jae
