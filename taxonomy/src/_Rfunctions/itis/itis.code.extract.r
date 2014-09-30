

  itis.code.extract = function ( DS, value, tx=NULL ) { 
    
    # generic data accessor function
    # the itis taxonomy db (tx) can optionally be sent from calling function to speed things up 
    # as this loading step is slow 
    
    out = NULL
    
    value = tolower(value)

    if (DS=="kingdom") {
      k = itis.db( "kingdoms" )
      out = k$kingdom_id[ which( tolower(k$kingdom_name) == value ) ]
    }
    
    if (DS=="taxon.unit.types") {
      tx = itis.db( "taxon.unit.types" )
      out = sort( unique( tx$rank_id[ which( tolower(tx$rank_name)==value ) ] ))
    }

    if (DS=="itaxa") {
      # this is slow as the loading of itis.db("itaxa") is slow
      if (is.null(tx)) tx = itis.db( "itaxa" )
      out1 = itis.taxa.to.tsn( tx=value, itaxa=tx )
      out2 = itis.vernacular.to.tsn( tx=value, itaxa=tx )  # vernaculars are checked as sometimes they have other scientific names
      out = na.omit( sort( unique( c( out1, out2 ) ) ) )
    }
    
    if (DS=="taxa.vernacular") {
      # this is slow as the loading of itis.db("itaxa") is slow
      if (is.null(tx)) tx = itis.db( "itaxa" )
      out = itis.vernacular.to.tsn( tx=value, itaxa=tx )
      out = na.omit( out )  # first one is the best guess
    }
    

    return(out)
  }


