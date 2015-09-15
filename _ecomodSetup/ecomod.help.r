
ecomod.help = function( h="ecomod.help", filepattern="\\.r$") {
  #// ecomod.help( "ecomod function name" ) - searches for help inside a function
   
  fn.docs = file.path( ecomod.workdirectory, "ecomod.help.docs.rdata")
  fn.code = file.path( ecomod.workdirectory, "ecomod.help.sourcecode.rdata" )
  
  if (h=="refresh") {
    #// ecomod.help( "refresh" ) - refresh list of ecomod functions available locally
    print( "Refreshing locally available list of function names ... ")
    fl = NULL
    fl = list.files( path=ecomod.directory, pattern=filepattern, full.names=TRUE, 
                     recursive=TRUE,  ignore.case=TRUE, include.dirs=FALSE )
    code = list()
    docs = list()
    for ( f in fl ) {
      o = scan( f, "character", sep="\n", quiet=TRUE ) 
      k = try( grep ( "\\#\\/\\/", o, ignore.case=TRUE  ), silent=TRUE )
      res = "No help .. please add"
      if (! (class(k) %in% "try-error")) {
        if ( length(k) > 0) {
          res = gsub( "^[[:space:]]*\\#\\/\\/[[:space:]]*", "", o[k] )
          res = list( res )
      }}
      docs[f] = res 
      code[f] = list( o )
    }
    save( docs, file=fn.docs, compress=TRUE )
    save( code, file=fn.code, compress=TRUE )
    print( paste( "Local help files saved to ", ecomod.workdirectory ))
  }

  if ( !file.exists( fn.docs) | !file.exists( fn.code) ) ecomod.help( "refresh" )
  load( fn.docs )

  docs.names = names( docs)

  mm = grep( h, docs.names, ignore.case=TRUE )
  if (length(mm) == 0) return( "Function not found. Try refreshing local help with: ecomod.help('refresh')) ")

    cat("\n")
    cat( "Function is found at:" )
    cat("\n\n")
    cat( paste( " ", docs.names[mm], collapse="\n") )
    cat( "\n\n" )
    cat( "Help documentation:")
    cat( "\n\n  " )

    out = paste( docs[[mm]],  collapse="\n  "  )
    cat( ( out) )
    cat( "\n" )
    cat( "\n" )
}


