
  # determine home directory (homedir) 
	
	if (! exists("jags.exe") ) {
      
    OSinfo = Sys.info()
    OS = OSinfo["sysname"]
    
    if ( OS == "Linux"  ) {
      jags.exe = file.path("/usr", "bin", "jags")
    
    } else if ( OS == "Windows" ) {
      jags.exe = file.path( "C:", "Program Files", "JAGS", "jags.exe" )  # <<- change here 
    } 

  }

	if (is.null ( jags.exe ) ) {
		# final check
    stop( "You will have to define the location of the JAGS executable using:\n option(jags.exe=file.path(...))" )
	}
	
	options( jags.exe=jags.exe )

