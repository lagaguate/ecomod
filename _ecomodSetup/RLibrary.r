	RLibrary = function( ... ) {
	  # used to load libraries conveniently 
		for ( l in c(...) ) require( l, character.only=T )
	  return( c(...) )
  }
	
