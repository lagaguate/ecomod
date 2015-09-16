
ecomod.search = function(  h="ecomod.help", ... ) {
  #// ecomod.search ( "keyword", ... )  -- '...' are grep options
  #// ecomod.search ( "^eco.*" )  -- keyword can be REGEX .. start a line with "eco"
 
  if ( h=="ecomod.help" ) {
    ecomod.help( "ecomod.search" )
    return("Pass keyword or a regular expression (?regex)") 
  }

  if ( !file.exists( fn.code) ) ecomod.help( "refresh" )
  fn.code = file.path( ecomod.workdirectory, "ecomod.help.sourcecode.rdata" )
  load( fn.code )

    extractData = function(X, h, ... ) { 
      ee = grep( h, X, ignore.case=TRUE, ... )
      out = NULL
      if (length(ee) >0 ) {
        out= paste(  paste( names(X), "[", ee, "]", sep=""), X[ee] )
      }
      return(out)
    }

  res = sapply( code, extractData, h=h, ... )
  rr = which( lapply( res, length ) > 0)

  if (length( rr) > 0 ) {
    cat( "\nFile names and [line number] where matches were found: \n" )
    cat( "\n" )
    for ( r in rr ) {
      output = res[r]
      cat( paste( names( output), ":" ))
      cat("\n\n")
      cat( paste(" ", unlist(output), sep="\n" ) )
      cat("\n\n")
    }
  } else {
    cat( "No matches were found \n") 
  }

}


