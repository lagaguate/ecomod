
This is a code repository of various functions, scripts and tools useful for general ecological modelling, population stock assessments and habitat analysis. The code is mostly implemented in the free and open-source R language/environment (http://www.r-project.org/). There is some minimal reliance upon other languages such as Python, JAGS/BUGS, GMT (Generic Mapping Tools), JAVA, Fortran, C, etc., these are being gradually removed.

Some of these subprojects are inter-related in that they refer to each other's functions and data streams. Others are didactic, demonstrating approach or simply documenting the methods (especially as many data sets are not publicly available). However, all the subprojects are here to further collaboration, communication and transparency, to reduce the replication of effort and to permit other researchers to apply these methods and improve upon them. 

Rather than implementing the repository as 'R-packages', we use the GIT version control system as it permits rapid and distributed development of code. In other words, everything here is a work-in-progress and subject to breakage and documentation will be in some ways lacking. (But see below for accessing some internal documentation/help). Mostly, you will have to follow the example scripts to help you understand usage and functionality. Nonethless, the Master branch visible here is as near to production-level code as possible. It serves as the operational basis of real research and applied science programs at the Bedford Institute of Oceanography, Canada.

To download this repository, you can either:

  1. Get the current snapshot of the whole repository (zip-compressed; see link near top), or 
  2. Clone it using git (for help see http://git-scm.com/book/): 
       a) At a command prompt: git clone http://github.com/jae0/ecomod.git 
       b) Or if using MSWindows, you optionally use 'Msysgit' )  

To use ecomod, the following standard for directory structures for source code and data must be adhered to:

    ecomod ("ecomod.directory", see below)
        |___ project name 1 
      	    |___ src (location of main scripts, sequentially numbered if sequence is important )
      	        |___  _Rfunctions (location of functions supporting the project, there can be subdirectories if desired) 
        |___ project name 2 (etc)
      
    ecomod_data ("ecomod.datadirectory", see below)
        |___ project name 1 
      	    |___ (various subdirectories as required by the project)
      	    |___ (etc)
        |___ project name 2 (etc)


The above root directories must be defined using the following key variable names:

    ecomod.directory = file.path( "C:", "path", "to", "ecomod" )           ### replace with correct path to local ecomod directory 
    ecomod.datadirectory = file.path( "C:", "path", "to", "ecomod_data" )  ### replace with correct path to local data directory 
    

    source( file.path( ecomod.directory, "_ecomodSetup", "ecomod.rprofile.r" ) ) ### initializes the ecomod environment

They can be defined where you prefer. It is probably simplest to define them in your .Rprofile file (located in your 'home' directory. If this file does not already exist, simply create a new file. In MSWindows, the home directory seems to be the "My Documents" directory (i.e. C:\Users\<user name>\Documents). If not known, run R and then type: getwd(). In Linux and other UNIX variants, including MacOSX it is your usual home directory.) 


Example usage 

"loadfunctions()" is a simple function that reads in all files under a particular project directory in a structured manner. 
To load in all functions under the ecomod/common/src/_Rfunctions/ and ecomod/snowcrab/src/_Rfunctions/ you need to run the following:

    loadfunctions( c("common", "snowcrab") )  

or to load a specific file/function that fuzzy matches the 'functionname' criterion under the directory snowcrab/src/

    loadfunctions( "snowcrab", functionname="current.assessment.year.r")  
    
In addition, the following will provide a simple way of referring to the directory structure of a given project:

    project.codedirectory( "spacetime" ) # returns the file path to the spacetime code directory under ecomod
    project.datadirectory( "spacetime" ) # returns the file path to the spacetime data directory under ecomod_data (or whatever you defined as the root data location in your Rprofile )

To search for functions or key words with ecomod, use the following:

    ecomod.help( "functionname" ) # find help for a given function ( anything that follows:  #// if you plan to add documentation )
    ecomod.search( "habitat" )    # search for every file for 'habitat' and return file name and line number
    

