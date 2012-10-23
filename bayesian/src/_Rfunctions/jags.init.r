	
	if (is.null ( jags.exe ) ) {
		stop( "You will have to define the location of the JAGS executable using: option(jags.exe=file.path(...))" )
	}
	
	options( jags.exe=jags.exe )

