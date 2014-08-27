   
  transf.lookup = function( variable, db ) {
    
    tl = tranf.db.lookup(db) 

    if (file.exists( tl$repository) ) {
      load (tl$repository)
    } else {
      REPOS = recode.variable.initiate.db ( db )
    }
    
    ii = which( REPOS$varname == variable )
    if (length(ii) == 0 ) { # missing from list .. print error message and stop to figure out why
      print( paste("Recode:", variable, "is missing from database, assuming no transformation"))
      tmp = REPOS[1,]
      tmp[1,] = c(variable, "none", 0, 1 )
      return( tmp ) 
    } else if ( length(ii) > 1 ) { 
      print(paste( "Error in recode:", variable, "has too many incidences in database"))
      stop()
    } 
    
    return( REPOS[ii ,] )
  }

