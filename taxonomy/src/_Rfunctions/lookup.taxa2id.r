

  lookup.taxa2id = function( tx ) {

    out = list()
		itaxa = itis.db()
    spi = taxa.db() 

    for ( i in 1:length(tx) ) {

			word = tx[i]
      word = strip.unnecessary.characters(word)

			tsn0 = itis.taxa.to.tsn(  tx=word, itaxa=itaxa )
			tsn1 = itis.vernacular.to.tsn(  tx=word, itaxa=itaxa )
			tsn = sort( unique( c(tsn0, tsn1))) 

			spec= lookup.taxa2spec( tx=word, spi=spi )

      # cross-reference itis and taxa.db id's
			
			if ( length(spec) > 0 ) {
				tsn.add = spi$itis.tsn[ which( spi$spec %in% spec) ]
				tsn = sort( unique( c( tsn, tsn.add) ) )
      } 
			
			if ( length(tsn) > 0 ) {
				spec.add = spi$spec [which( spi$itis.tsn %in% tsn) ]
				spec = sort( unique( c( spec, spec.add) ) )
			}

			out[[i]] = list(taxa=tx[i], tsn=tsn, spec=spec )

    }
    return (out) 
  }


