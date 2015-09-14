
ecomod.help = function( h=NULL, allUsage=FALSE, filepattern="\\.r$" ) {
  #// ecomod.help( "search item" ) - searches for help inside a function
  
  if (is.null(h)) ecomod.help( "ecomod.help" )
  
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
  }

  
  if ( !file.exists( fn.docs) | !file.exists( fn.code) ) ecomod.help( "refresh" )
  
  load( fn.docs )
  load( fn.code )

  mm = grep( h, names( docs), ignore.case=TRUE )
  if (length(mm) > 0 ) {
    cat("\n")
    cat( "Function is found at:" )
    cat("\n\n")
    cat( names( docs)[mm] )
    cat( "\n\n" )
    cat( "Help documentation:")
    cat( "\n\n  " )
    cat( paste( docs[[mm]], collapse="\n  "  ), "\n" ) 
  }

  if (length(mm)==0 | allUsage ) {
    extractData = function(X, h, ... ) { 
      ee = grep( h, X, ignore.case=TRUE, ... )
      out = NULL
      if (length(ee) >0 ) out = X[ee]
      return(out)
    }
    res = lapply( code, extractData, h=h )
    rr = which(sapply( res, length ) > 0)

    output = "No matches" 
    if (length( rr) > 0 )  output = res[rr]
    
    cat( "\nFiles and [line numbers] where matches were found:\n" )
    print(output)

  }
}


