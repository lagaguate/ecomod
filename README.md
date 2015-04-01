
This is a code repository of various functions, scripts and tools useful for general ecological modelling, population stock assessments and habitat analysis. The code is mostly implemented in the R-language (http://www.r-project.org/) and intended to be run through the free and open-source R-environment. There is some occasional (minimal) reliance upon other languages such as python, JAGS/BUGS, GMT (Generic Mapping Tools), JAVA, Fortran, C, etc. 

Some of these subprojects are inter-related in that they refer to each other's functions and data streams. Others are didactic, demonstrating approach or simply documenting the methods (especially as many data sets are not publicly available). However, all the subprojects are here to further collaboration, communication and transparency, to reduce the replication of effort and to permit other researchers to apply these methods and improve upon them. 

Rather than implementing the repository as 'R-packages', we use the GIT version control system as it permits rapid and distributed development of code. In other words, everything here is a work-in-progress and subject to breakage and documentation will be in some ways lacking. You will have to follow the example scripts to help you understand usage and functionality. Nonethless, the Master branch visible here is as near to production-level code as possible. It serves as the operational basis of real research and applied science programs at the Bedford Institute of Oceanography, Canada.

To download this repository, you can either:

  1. Get the current snapshot of the whole repository (zip-compressed; see link near top), or 
  2. Clone it using git (for help see http://git-scm.com/book/): 
       a) At a command prompt: git clone http://github.com/jae0/ecomod.git 
       b) Or if using MSWindows, you optionally use 'Msysgit' )  


To fully use this respository, you will need to add the following to the end of your .Rprofile file (located in your 'home' directory).  If this file does not already exist, simply create a new file and add the following:

    ecomod.workdirectory = file.path( "C:", "R", "workspace" )    ### replace with correct path to R workspace
    ecomod.datadirectory = file.path( "C:", "path", "to", "ecomod_data" )  ### replace with correct path to local data directory ... must have the same directory hierarchy
    ecomod.directory = file.path( "C:", "path", "to", "ecomod" )  ### replace with correct path to cloned folder
	
    source( file.path( ecomod.directory, "_ecomodSetup", "ecomod.rprofile.r" ) ) ### initializes the ecomod environment

In MSWindows, the home directory seems to be the "My Documents" directory (i.e. C:\Users\<user name>\Documents). But to be sure, run R and then type getwd(). In Linux and other UNIX variants, including MacOSX it is your usual home directory. 


Example usage 

To load in all functions under the common/src/_Rfunctions/ and snowcrab/src/_Rfunctions/ you need to run the following:

    loadfunctions( c("common", "snowcrab") )  

or to load a specific file/function that fuzzy matches the 'functionname' criterion under the directory snowcrab/src/

    loadfunctions( "snowcrab", functionname="current.assessment.year.r")  
    

'loadfunctions()' is a simple function that reads in all files under a particular project directory in a structured manner. 

    project.codedirectory( "spacetime" ) # returns the file path to the spacetime code directory under ecomod
    project.directory( "spacetime" ) # returns the file path to the spacetime data directory under ecomod_data (or whatever you defined as the root data location in your Rprofile )
    project.datadirectory()  is a synomym for project.directory() ... the latter is depricated 


