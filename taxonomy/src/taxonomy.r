

  
 	loadfunctions( c("groundfish", "common", "taxonomy") ) 
 
  require ( multicore ) # simple parallel interface (using threads)


  if ( refresh.itis.tables ) {
    itis.db( "make.snapshot", lnk="http://www.itis.gov/downloads/itisMySQLTables.tar.gz") 
    itis.db( "main.redo")     # merge data and handle duplicates
  }


  if ( refresh.bio.species.codes ) {
    
    taxa.db( "spcodes.odbc.redo" ) # refresh BIO's species codes from Oracle -- done also from groundfish update
    groundfish.db( DS="gscat" ) # creates intermediate files to update species list
		# merge in itis taxonomy tsn's ... use parallel=F to debug
    taxa.db( "spcodes.itis.redo" )  
    
    # merge full taxonomic hierrachy (limit to animalia and resolved to species)
    taxa.db( "full.taxonomy.redo" ) 
    taxa.db( "life.history.redo" ) # add life history data (locally maintained in gstaxa_working.csv )
    taxa.db( "complete.redo" )
		
		specieslist.parsimony("default.redo")  # update lookups
		
 
    # not used (.. delete? .. yes )
    # itis.groundfish = taxa.db( "itis.groundfish.redo" )
    # itis.observer = taxa.db( "itis.observer.redo" )
   
  }



    # -------------------------------
    # example usage to extract TSN's
		
		tx="Microgadus tomcod"
			
			lookup.taxa2spec (tx) # lookup only from local taxa db
			lookup.taxa2id (tx)  # look up species id from both itis and local taxa.db
			itis.taxa.to.tsn( tx) # look up only from itis

			o = lookup.taxa2id (tx)
				o
				lookup.spec2taxa( o[[1]]$spec ) 
				lookup.tsn2taxa( o[[1]]$tsn ) 
				itis.extract( o[[1]]$tsn[1], taxa)


		lookup.spec2taxa( c(10, 20) )  # look up species names
		
		itaxa = itis.db( "itaxa" ) 
    
		tx = "Microgadus tomcod"
			itis.taxa.to.tsn(  tx=tx, itaxa=itaxa )
    
    tx = "ling"
			txids = itis.vernacular.to.tsn( tx=tx, itaxa=itaxa )
      lookup.tsn2taxa(txids) 

    # ITIS data from tsn's
    tsn = c(164714:164780)
			itis.extract( tsn, itaxa)

    # -------------------------------
    
    kid = itis.code.extract( DS="kingdom", value="Animalia") 
    tid = itis.code.extract( DS="taxon.unit.types", value="species" )
    sid = itis.code.extract( DS="itaxa", value="Gadus morhua" )
    sid = itis.code.extract( DS="itaxa.vernacular", value="atlantic cod" )



   

