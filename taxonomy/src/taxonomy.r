

  # taxonomy.db contains taxa/species codes that are internally (ecomod) consistent and parsimonious 
 
  # require ( multicore ) # simple parallel interface (using threads) .. does not work well in MSWindows?
 	
  loadfunctions( c("groundfish", "utility", "taxonomy") ) 
 

  refresh.itis.tables = FALSE
  if ( refresh.itis.tables ) {
    itis.db( "make.snapshot", lnk="http://www.itis.gov/downloads/itisMySQLTables.tar.gz") 
    itis.db( "main.redo")     # assemble all itis tables into a coherent and usable form  
  }


  bootstrap.new.data.system = FALSE  
  if ( bootstrap.new.data.system) {
      
    # first. refresh BIO's species codes from Oracle -- done also from groundfish update
    taxonomy.db( "spcodes.redo" )  
    
    # bootstrap an initial set of tables .. these will be incomplete as a parsimonious tree needs to be created first but it depends upon the last file created taxonomy.db("complete") .. so ...
    taxonomy.db( "groundfish.itis.redo" )  ## link itis with groundfish tables using taxa names, vernacular, etc
    taxonomy.db( "full.taxonomy.redo" )  # merge full taxonomic hierrachy (limit to animalia and resolved to species)
		## taxonomy.db( "parsimonious.redo" )  # (re)create lookups from old codes to a parsimonious species list
    taxonomy.db( "life.history.redo" ) # add life history data (locally maintained in groundfish.lifehistory.manually.maintained.csv )
    taxonomy.db( "complete.redo" )
    taxonomy.db( "parsimonious.redo" ) 
  }


  example.usage = FALSE
  if (example.usage) {

    # -------------------------------
    # example usage to extract TSN's
		
		tx="Microgadus tomcod"
		
    tx="cod"

			taxonomy.recode( from="taxa.fast", tolookup=tx) # lookup only from local taxonomy db
			taxonomy.recode( from="taxa", tolookup=tx ) # look up species id from both itis and local taxonomy.db
			
      lu = taxonomy.recode( from="taxa", tolookup="AMMODYTES" ) # look up species id from both itis and local taxonomy.db
      taxonomy.recode( from="tsn", to="taxa", tolookup= lu[[1]]$tsn )

      taxonomy.recode( from="spec", to="taxa", tolookup=taxonomy.recode( from="taxa.fast", tolookup="AMMODYTES" ) )


      itis.taxa.to.tsn( tx) # look up only from itis

			o = taxonomy.recode( from="taxa", tolookup=tx )
				o
				taxonomy.recode( from="spec", tolookup=o[[1]]$spec ) 
				taxonomy.recode( from="tsn", to="taxa", tolookup= o[[1]]$tsn ) 
				itis.extract( o[[1]]$tsn[1], itis.db( "itaxa" ))


		taxonomy.recode( from="spec", tolookup=c(10,20) ) 
				
		itaxa = itis.db( "itaxa" ) 
    
		tx = "Microgadus tomcod"
			itis.taxa.to.tsn(  tx=tx, itaxa=itaxa )
    
    tx = "ling"
			txids = itis.vernacular.to.tsn( tx=tx, itaxa=itaxa )
      taxonomy.recode( from="tsn", to="taxa", tolookup=txids ) 

    # ITIS data from tsn's
    tsn = c(164714:164780)
			itis.extract( tsn, itaxa)

    # -------------------------------
    
    kid = itis.code.extract( DS="kingdom", value="Animalia") 
    tid = itis.code.extract( DS="taxon.unit.types", value="species" )
    sid = itis.code.extract( DS="itaxa", value="Gadus morhua" )
    sid = itis.code.extract( DS="itaxa.vernacular", value="atlantic cod" )


  }
   

