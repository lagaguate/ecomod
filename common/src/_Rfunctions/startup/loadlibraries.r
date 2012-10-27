	# used to load libraries conveniently
	loadlibraries = function( ... ) {
		for ( l in c(...) ) require( l, character.only=T )
	}
	
