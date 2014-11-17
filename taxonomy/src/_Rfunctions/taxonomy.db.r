

  taxonomy.db = function( DS="complete", itis.taxa.lowest="species" ) {
    
    taxadir.manually.maintained =  file.path(project.directory( "taxonomy"), "data.manually.maintained" )  # part of the respository
    taxadir = file.path(project.directory( "taxonomy"), "data" )
    localdir = file.path(project.directory( "taxonomy"), "data", "data.locally.generated" )

    dir.create( taxadir.manually.maintained, recursive=TRUE, showWarnings=FALSE )
    dir.create( taxadir, recursive=TRUE, showWarnings=FALSE )
    dir.create( localdir, recursive=TRUE, showWarnings=FALSE )

    if ( DS == "gstaxa" ) return( taxonomy.db( "life.history") )  

    if ( DS %in% c("spcodes", "spcodes.redo"  ) ) {
      # this is just a synonym for the groundfish data dump : 
      if ( DS == "spcodes" ) {
        spcodes = groundfish.db( DS="spcodes.odbc" ) 
        return( spcodes )
      }
      
      groundfish.db( DS="spcodes.odbc.redo" ) 
      return ( "Complete" )
    }


   
    # ------------------------------


 
    if (DS %in% c("groundfish.itis.redo", "groundfish.itis" )) {
      
			print( "Warning:  ")
      print( "  spec = bio species codes -- use this to match data but not analysis" )
			print( "  spec.clean = manually updated codes to use for taxonomic work in groundfish.itis.lookuptable.manually.maintained.csv" )
			print( "" )
			
			# add itis tsn's to spcodes -- this completes the lookup table
			# a partial lookup table exists and is maintained locally but then is added to using 
			# text matching methods, which are a bit slow.

      fn = file.path( localdir, "lookup.groundfish.itis.rdata" )
      
      if ( DS =="groundfish.itis") {
        spi = NULL
        if (file.exists(fn)) load(fn)
        return (spi)
      }
      
			print( "Merging ITIS and Groundfish species codes using taxa or vernacular names ...")
      
      # load groundfish species codes 
      spi = taxonomy.db( DS="spcodes" )  
      names(spi) = tolower( names(spi) )
      
      spi$name.common = as.character( spi$comm )
      spi$name.scientific = as.character( spi$spec )
      spi$spec = as.numeric(spi$code)
      spi = spi[ , c("spec", "name.common", "name.scientific" ) ]
  	

			# update tsn's directly using any local corrections/additions
			# these have been manually verified: http://www.itis.gov/servlet/SingleRpt/SingleRpt
			# these also help speed up the lookup through itis as that is slow
			print( "Updating from manually verified tsn/spec codes:")
			print( "maintained in file: groundfish.itis.lookuptable.manually.maintained.csv" ) 
      print( "      -- must be'|' delimited and 'quotes used for export' " )
			print( "### New additions can be placed here too " )
			
			fn.local.taxa.lookup = file.path( taxadir.manually.maintained, "groundfish.itis.lookuptable.manually.maintained.csv" )
			tx.local = read.csv( file=fn.local.taxa.lookup, sep="|", as.is=T, strip.white=T, header=T, fill=T) 
			tx.local = tx.local[, c("spec", "spec.clean", "accepted_tsn", "name.common.bio", "comments" )]
      
      tx.local = tx.local[  which( is.finite( tx.local$spec.clean ) ) ,]
      
      oo =  which( !is.finite( tx.local$spec ) ) 
      tx.local$spec[oo] = tx.local$spec.clean[oo]  # overwrite missing with new species id's ( == spec.clean == - itis.tsn )

      tx.local = tx.local[ which( is.finite( tx.local$spec ) ) ,]
			it = which( duplicated( tx.local$spec))
			if (length(it)>0) {
				print ( "Warning: Duplicated spec codes found in groundfish.itis.lookuptable.manually.maintained.csv")
				print ( tx.local[ which(tx.local$spec %in% unique(tx.local$spec[it]) ), ])
				tx.local = tx.local[ -it, ]
			}

			# this adds new species to the local lookup
      spi.n0 = nrow(spi)
			spi_id = unique( spi$spec )
			spi = merge( spi, tx.local, by="spec", all.x=T, all.y=T, sort=F )

			# overwrite completely as this is the first data layer for the tsn's
			spi$itis.tsn = spi$itis.tsn_manually_maintained = spi$accepted_tsn
			spi$accepted_tsn = NULL

      # additional vars to help with lookup
      spi$tolookup = TRUE
      spi$flag = ""


      # items to drop identified in groundfish.itis.lookuptable.manually.maintained.csv 
			# mark items to not be looked up
			ib = which( spi$itis.tsn == -1  )
			if (length (ib)>0 ) {
				spi$tolookup[ ib] = FALSE
				spi$flag[ib] = "manually rejected"
				spi$itis.tsn[ ib] = NA  # reset to NA
			}

			lu = which( is.finite(spi$itis.tsn))
			spi$tolookup[lu] = FALSE
			spi$flag[lu] = "manually determined"


      # check for keywords that flag that no lookup is necessary
      spi = taxonomy.keywords.flag( spi, "name.scientific" )
      spi = taxonomy.keywords.flag( spi, "name.common" )
      spi = taxonomy.keywords.flag( spi, "name.common.bio" )

      # remove words with punctuation
      spi = taxonomy.keywords.remove( spi, "name.scientific", withpunctuation=T )
      spi = taxonomy.keywords.remove( spi, "name.common", withpunctuation=T )
      spi = taxonomy.keywords.remove( spi, "name.common.bio", withpunctuation=T )

      # remove words without punctuation
      spi = taxonomy.keywords.remove( spi, "name.scientific", withpunctuation=F )
      spi = taxonomy.keywords.remove( spi, "name.common", withpunctuation=F )
      spi = taxonomy.keywords.remove( spi, "name.common.bio", withpunctuation=F )

      # final formatting of names
      spi$name.scientific = taxonomy.strip.unnecessary.characters(spi$name.scientific)
      spi$name.common = taxonomy.strip.unnecessary.characters(spi$name.common)
      spi$name.common.bio = taxonomy.strip.unnecessary.characters(spi$name.common.bio)

   

      # link itis with groundfish species codes using an exhaustive search of all taxa names
      vnames = c( "name.scientific", "name.scientific", "name.common", "name.common.bio" )
      vtypes = c( "default", "vernacular", "vernacular", "default" )
      spi = itis.lookup.exhaustive.search( spi, vnames, vtypes )  


      # fill in missing names, etc
      i = which(is.na( spi$name.scientific))
      if (length(i) > 0 ) {
        oo = taxonomy.recode( from="tsn", to="sci", tolookup=spi$itis.tsn[i] )
        if (length(oo) == length(i)) spi$name.scientific[i] = oo
      }
      
      i = which(is.na( spi$name.common))
      if (length(i) > 0 ) {
        oo = taxonomy.recode( from="tsn", to="tx", tolookup=spi$itis.tsn[i] )
        if (length(oo) == length(i)) spi$name.common[i] = oo        
      }

      # have to do it again and fill with scientific name if missing
      i = which(is.na( spi$name.common))
      if (length(i) > 0 ) {
        oo = taxonomy.recode( from="tsn", to="sci", tolookup=spi$itis.tsn[i] )
        if (length(oo) == length(i)) spi$name.common[i] = oo
      }
    
      i = which(is.na( spi$name.common.bio))
      if (length(i) > 0 ) {
        oo = taxonomy.recode( from="tsn", to="tx", tolookup=spi$itis.tsn[i] )
        if (length(oo) == length(i)) spi$name.common.bio[i] = oo
      }

      # have to do it again and fill with scientific name if missing
      i = which(is.na( spi$name.common.bio))
      if (length(i) > 0 ) {
        oo = taxonomy.recode( from="tsn", to="sci", tolookup=spi$itis.tsn[i] )
        if (length(oo) == length(i)) spi$name.common.bio[i] = oo
      }

  		# make sure the remainder of missing spec.clean points to spec
			i = which( !is.finite(spi$spec.clean) )
			if (length(i)>0) {
        spi$spec.clean[i] = spi$spec[i]
      }
      
      # now that tsn lookup's have been completed, it is necessary to update 
      # the tsn's to reflect any manually determined spec.clean from tx.local
			i = which( (spi$spec != spi$spec.clean) & spi$tolookup )
			if (length(i)>0) {
				for (j in i) {
					k = which( spi$spec== spi$spec.clean[j] )
          if (length(k)>0) spi$itis.tsn[ j ] = spi$itis.tsn[ k ]
				}
			}

  		# for all duplicated tsn's, point them to the same species id:   
      # spec.clean id's to point to a single spec id (choose min value as default)
			dup.tsn = sort( unique( spi$itis.tsn[ duplicates.toremove( spi$itis.tsn )]  )) 
			if (length( dup.tsn) > 0 ) {
        for (tsni in dup.tsn) {
          oi = which( spi$itis.tsn ==tsni )
          op = which.min( spi$spec[ oi ] )
          spi$spec.clean[oi] = min( spi$spec[oi[op]],  spi$spec[oi] )
        }
      }


			i = which( !is.finite(spi$itis.tsn) & spi$tolookup )
			if (length(i)>0) {
				print( "The following have no itis tsn matches ")
				print( "for now, assuming their spec id's are OK" )
				print( "Their tsn's should be manually identified and updated in the local updates file:" )
				print( "groundfish.itis.lookuptable.manually.maintained.csv")
        print( "  -- see groundfish.itis.redo, above .. these are stored in with '|' as delimiter " ) 
				print( spi[i,] )
				fn2 = file.path( taxadir, "spcodes.no.itis.matches.csv" )
				print (fn2 )
				write.csv ( spi[i,], file=fn2 )
			}

      save( spi, file=fn, compress=T )

      return ( fn )
    } 


    # ------------------

    if (DS %in% c("full.taxonomy", "full.taxonomy.redo") ) {
      
      # add full taxonomic hierarchy to spcodes database .. 
	
      require ( parallel ) # simple parallel interface (using threads)
		
      itis.taxa.lowest = tolower(itis.taxa.lowest)
      fn = file.path( localdir, paste("spcodes.full.taxonomy", itis.taxa.lowest, "rdata", sep=".") )
      
      if (DS=="full.taxonomy") {
        spf = NULL
        if (file.exists(fn)) load(fn)
        return(spf)
      }

      spf = taxonomy.db( DS="groundfish.itis" )
      itaxa = itis.db( "itaxa" )
      tunits =  itis.db( "taxon.unit.types" )
      kingdom =  itis.db( "kingdoms" )

      # reduce memory requirements: drop information below some taxonomic level
      tx_id = unique( tunits$rank_id[ which( tolower(tunits$rank_name)==itis.taxa.lowest ) ] )
      tunits = tunits[ which( tunits$rank_id <= tx_id ) , ]
			tunits = tunits[ -which(duplicated(tunits$rank_id)) ,]
			tunits = tunits[ order( tunits$rank_id) , ]
      tunits = merge( tunits, kingdom, by="kingdom_id", sort=F ) 

      
      fd = which( duplicated ( tunits$rank_name) )
      if ( length(fd) > 0 ) {
        for (fi in fd ) {
          d0 = which( tunits$rank_name == tunits$rank_name[ fi] )
          tunits$rank_name[d0] = paste( tunits$kingdom_name[d0], tunits$rank_name[d0],sep=".")
        }
      }

      test = NULL
      while ( is.null( test) | is.null(names(test)) ) {
        test =  itis.format( sample(nrow(spf), 1), tsn=spf$itis.tsn, itaxa=itaxa, tunits=tunits )
      }
      formatted.names = names( test )

			debug = F
			if (debug) {
				out = matrix(NA, ncol=length(test), nrow=nrow(spf) )
				for ( i in 1:nrow(spf) ) {
					o = itis.format( i, tsn=spf$itis.tsn, itaxa=itaxa, tunits=tunits )
					print (o)
					out[i,] = o
				}
			}
     
      print( "Extracting full taxonomy" ) 
      res = list()
      for (i in 1:nrow(spf) ) {
        print(i)
        res[[i]] = itis.format( i=i, tsn=spf$itis.tsn, itaxa=itaxa, tunits=tunits )
      }

      res = unlist( res)
      res = as.data.frame( matrix( res, nrow=nrow(spf), ncol=length(formatted.names), byrow=T ), stringsAsFactors=F )
      colnames(res) = tolower(formatted.names)
      res = res[order(res$rowindex ) ,]

      # mclapply method
			# res = mclapply( 1:nrow(spf), itis.format, tsn=spf$itis.tsn, itaxa=itaxa, tunits=tunits )
      # res = unlist( res) 
      # res = as.data.frame( matrix( res, nrow=nrow(spf), ncol=length(formatted.names), byrow=T ), stringsAsFactors=F )
      # colnames(res) = tolower(formatted.names)

      spf = cbind( spf, res )
      spf$rowindex = NULL

      save( spf, file=fn, compress=T )
      return ( fn )
    
    }
  
    
    
    # ------------------


    if (DS %in% c( "life.history", "life.history.redo") ) {
      
      fn = file.path( localdir, "spcodes.lifehistory.rdata") 
      fn.local = file.path( taxadir.manually.maintained, "groundfish.lifehistory.manually.maintained.csv") 
      
      if (DS == "life.history" ) {
        sps = NULL
        if (file.exists(fn)) load(fn)
        return(sps)
      }

      print( "Local files are manually maintained.")
      print( "Export to CSV if they have been updated to with '|' as a delimiter and remove last xx lines:" )
      print( fn.local )

      lifehist = read.csv( fn.local, sep="|", as.is=T, strip.white=T, header=T, stringsAsFactors=F ) 
      names( lifehist ) = tolower( names( lifehist ) ) 
			lifehist$spec = as.numeric( as.character( lifehist$spec ))
      
			lifehist = lifehist[ - which( duplicated ( lifehist$spec ) ) , ]
			lifehist = lifehist[ which( is.finite( lifehist$spec ) ) , ]

      itis.taxa.lowest = tolower(itis.taxa.lowest)

      sps = taxonomy.db( DS="full.taxonomy", itis.taxa.lowest="species" )
      sps = merge( sps, lifehist, by="spec", all.x=T, all.y=T, sort=F) 
      
      sps$rank_id = as.numeric(  sps$rank_id  )

      sps$end = NULL  # dummy to force/check correct CVS import
      sps$subgenus = NULL
      sps$tribe = NULL

			ii = which( is.na( sps$name.scientific ) )
      sps$name.scientific[ii] = sps$name.common.worktable[ii]

			ii = which( is.na( sps$name.scientific ) )
      sps$name.scientific[ii] = sps$vernacular[ii]


			ii = which( is.na( sps$name.common ) )
			sps$name.common[ii] = sps$vernacular[ii]
      
			ii = which( is.na( sps$name.common ) )
			sps$name.common[ii] = sps$name.common.worktable[ii]

			ii = which( is.na( sps$name.scientific ) )
      sps$name.scientific[ii] = sps$name.common[ii]

			ii = which( is.na( sps$name.common ) )
      sps$name.common[ii] = sps$name.scientific[ii]

      last.check =  grep("per set", sps$name.common, ignore.case=T)
      if (length(last.check)>0) sps = sps[ -last.check , ]
     

      save( sps, file=fn, compress=T )
      return( fn )

    }


    # ----------------------------------------------

    if ( DS %in% c("complete", "complete.redo") ) {
		  fn = file.path( localdir, "spcodes.complete.rdata") 
  	  sps = NULL
			if (DS == "complete" ) {
        if (file.exists(fn)) load(fn)
        return(sps)
      }

			sp = NULL

			tx = taxonomy.db("life.history")
		
			spec = sort( unique( tx$spec) )

			sp.graph = network.igraph( spec=spec, tx=tx )
			sp.graph = network.statistics.igraph( g=sp.graph )  # add some stats about children, etc.
      spi = network.igraph.unpack( g=sp.graph )
		
      # go through the list of taxa and identify updated spec.clean id's
			n0 = length(spec)
			sp = data.frame( spec=spec ) 
			sp = merge( sp, tx, by="spec", all.x=T, all.y=F )
			jj = which( !is.finite( sp$spec.clean ) )
			sp$spec.clean[jj] = sp$spec[jj] 
			
			sps = merge( sp, spi[,c("id", "usage", "children.n","children", "sci", "parent" )], 
  			by.x="itis.tsn", by.y="id", all.x=T, all.y=F) 
	  
			save (sps, file=fn, compress=T)
			return ( fn )
		} 
		

    # ----------------------------------------------
 


    if (DS %in% c( "parsimonious",  "parsimonious.redo" )) {
      
      fn = file.path( localdir, "spcodes.parsimonious.rdata" )
     
      if ( DS =="parsimonious") {
 		    # determine the most parsimonious species list based upon know taxonomy/phylogeny and local species lists 
  	    spi = NULL
        if (file.exists(fn)) load(fn)
        return(spi)
      }
      
      # load groundfish species codes 
      spi = taxonomy.db("complete")
      spi$spec.parsimonious = spi$spec.clean  # initialize with current best codes which are found in spec.clean

      ranks = sort( unique( spi$rank_id ),decreasing=T )
      ranks = setdiff( ranks, 220 )  # 220 is the lowest level of discrimination (species)
      
      # search for single children and a parent, recode parent to child
      for ( r in ranks ) {
        oo = which( spi$rank_id == r )
        for ( o in oo ) {
          if ( is.finite(spi$children.n[o]) && spi$children.n[o] == 1) {
            # only child --> recode this spec to child's spec 
            newspec = which( spi$itis.tsn == spi$children[o] ) # multiple matches likely as this is a recursive process -- pick lowest taxa level == highest rank_id
            if (length( newspec) == 0 ) next()
            if (!is.finite( spi$rank_id[newspec] ) ) next()
            nsp = newspec[which.max( spi$rank_id[newspec] )]
            if ( o==nsp ) next()
            spi$spec.parsimonious[o] = spi$spec.parsimonious[nsp]
            spi$children[o] = spi$children[nsp]
            spi$children.n[o] = spi$children.n[nsp]
            print( paste(  "Updating species list::", spi$spec.parsimonious[o], spi$sci[o], "->", spi$spec.parsimonious[nsp], spi$sci[nsp] ) )
          }
        }
      }
      
      save( spi, file=fn, compress=T )

      return ( fn )
    } 

  }


