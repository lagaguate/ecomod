
parse.odf.file = function( fn ) {

  dta = readLines(fn )
  nheaderlines = grep( "-- DATA --", dta, fixed=T, useBytes=TRUE )
  header = dta[1: nheaderlines]
  
  Y = read.table( fn, skip=nheaderlines, as.is=T)

  params = parse.odf.parameter.header( header) 

  if ( nrow( params ) != ncol(Y) ) {
    print( "Parse error .. variable length mismatch" )
    print( fn )
    print( head(Y) )
    print( params )
    stop()
  }
  
  vnames = paste( params$pname, params$units, params$nullvalue, sep="_" )

  names( Y) = vnames
  return (Y)

}
 

