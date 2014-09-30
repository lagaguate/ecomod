
	taxonomy.strip.unnecessary.characters = function( word ) {
		word = gsub( "[[:punct:]]", " ", word, ignore.case=T )  # remove ()/, etc.
		word = gsub( "[[:space:]]{1,}", " ", word, ignore.case=T )  # remove ()/, etc.
		word = gsub( "[[:space:]]+$", "", word, ignore.case=T )  # remove ()/, etc.
		word = gsub( "^[[:space:]]+", "", word, ignore.case=T )  # remove ()/, etc.
		return (word)
	}



