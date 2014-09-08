

  taxa.db = function( DS="complete", itis.taxa.lowest="species", find.parsimonious.spec=TRUE, res=NULL ) {
      
    taxadir = project.directory( "taxonomy", "data" )
    dir.create( taxadir, recursive=TRUE, showWarnings=FALSE )

    if ( DS == "gstaxa" ) return( taxa.db( "life.history") )  

    if ( DS %in% c("spcodes", "spcodes.redo"  ) ) {
      
      if ( DS == "spcodes" ) {
        spcodes = groundfish.db( DS="spcodes.odbc" ) 
        return( spcodes )
      }
      
      groundfish.db( DS="spcodes.odbc.redo" ) 
      return ( fn )
    }


    if ( DS %in% c("gscat.update" ) ) {
      
      fn = file.path( taxadir, "taxa.gscat.update.rdata" )
      if ( is.null(res) ) {
        if (file.exists(fn) ) load(fn)
        return( res )
      }

      save( res, file=fn, compress=T)
      return ( fn )
    }

    

    if (DS %in% c("spcodes.itis", "spcodes.itis.redo", "spcodes.itis.parsimonious" )) {
      
			print( "" )
			print( "Warning:: spec = bio species codes -- use this to match data but not analysis" )
			print( "          spec.clean = manually updated codes to use for taxonomic work in gstaxa_taxonomy.csv" )
			print( "" )
			
			# add itis tsn's to spcodes -- this completes the lookup table
			# a partial lookup table exists and is maintained locally but then is added to using 
			# text matching methods, which are a bit slow.

      fn = file.path( taxadir, "spcodes.itis.rdata" )
      fnp = file.path( taxadir, "spcodes.itis.parsimonious.rdata" )
      
      if ( DS =="spcodes.itis") {
        load(fn)
        return (spi)
      }
      
      if ( DS =="spcodes.itis.parsimonious") {
        load(fnp)
        return (spi)
      }
     
      
      spi = taxa.db( DS="spcodes" )  # load groundfish table
      names(spi) = tolower( names(spi) )
      
      spi$name.common = as.character( spi$comm )
      spi$name.scientific = as.character( spi$spec )
      spi$spec = as.numeric(spi$code)
      spi = spi[ , c("spec", "name.common", "name.scientific" ) ]
  	

			# update tsn's directly using any local corrections/additions
			# these have been manually verified: http://www.itis.gov/servlet/SingleRpt/SingleRpt
			# these also help speed up the lookup through itis as that is slow
			print( "Updating from manually verified tsn/spec codes:")
			print( "maintained in file: gstaxa_taxonomy.csv -- must be'|' delimited and 'quotes used for export' " )
			print( "### New additions can be placed here too " )
			
			fn.local.taxa.lookup = file.path( taxadir, "gstaxa_taxonomy.csv" )
			tx.local = read.csv( file=fn.local.taxa.lookup, sep="|", as.is=T, strip.white=T, header=T, fill=T) 
			tx.local = tx.local[, c("spec", "spec.clean", "accepted_tsn", "name.common.bio", "comments" )]
      
      tx.local = tx.local[  which( is.finite( tx.local$spec.clean ) ) ,]
      
      oo =  which( !is.finite( tx.local$spec ) ) 
      tx.local$spec[oo] = tx.local$spec.clean[oo]  # overwrite missing with new species id's ( == spec.clean == - itis.tsn )

      tx.local = tx.local[ which( is.finite( tx.local$spec ) ) ,]
			it = which( duplicated( tx.local$spec))
			if (length(it)>0) {
				print ( "Warning: Duplicated spec codes found in gstaxa_taxonomy.csv")
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


      # items to drop identified in gstaxa_taxonomy.csv 
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
      spi = taxonomy.flag.keywords( spi, "name.scientific" )
      spi = taxonomy.flag.keywords( spi, "name.common" )
      spi = taxonomy.flag.keywords( spi, "name.common.bio" )

      # remove words with punctuation
      spi = taxonomy.keywords.remove( spi, "name.scientific", withpunctuation=T )
      spi = taxonomy.keywords.remove( spi, "name.common", withpunctuation=T )
      spi = taxonomy.keywords.remove( spi, "name.common.bio", withpunctuation=T )

      # remove words without punctuation
      spi = taxonomy.keywords.remove( spi, "name.scientific", withpunctuation=F )
      spi = taxonomy.keywords.remove( spi, "name.common", withpunctuation=F )
      spi = taxonomy.keywords.remove( spi, "name.common.bio", withpunctuation=F )

      # final formatting of names
      spi$name.scientific = strip.unnecessary.characters(spi$name.scientific)
      spi$name.common = strip.unnecessary.characters(spi$name.common)
      spi$name.common.bio = strip.unnecessary.characters(spi$name.common.bio)

   

      vnames = c( "name.scientific", "name.scientific", "name.common", "name.common.bio" )
      vtypes = c( "default", "vernacular", "vernacular", "default" )

      spi = itis.lookup.exhaustive.search( spi, vnames, vtypes )

       
      add.local.database.gscat = TRUE 
      if (add.local.database.gscat) {

        print("these additions will lag behind by a year unless a second update of gscat is performed" )
        print("after running taxa.db( DS='spcodes.itis.redo')" )

        res = taxa.db( DS="gscat.update" )  # created in groundfish.db( DS="gscat.redo"), ie. rerun this to update this snapshot
        oo = which( !duplicated( res$spec ) & res$tolookup )
        if (length(oo) >0 ) {
          res = res[ oo ,]
          nspi = names( spi)
          res$itis.tsn_manually_maintained = res$itis.tsn
          res$comments = "updated from gscat"
          res$name.common.bio = res$name.scientific = res$name.common = res$taxa
          res$spec.clean = NA
          spi = rbind( spi, res[, names(spi) ] )
          spi$todrop = FALSE

          pp = which( duplicated( spi$spec ) )
          if (length( pp) > 0 ) {
            for ( ip in pp ) {
              iip = which( spi$spec == spi$spec[ip] )
              if (length(iip) > 1) {
                print (spi[ iip, ] )
                tokeep = which( is.finite( spi$spec.clean[iip] ) | !is.finite(spi$itis.tsn_manually_maintained[iip]) )
                if ( length( tokeep)==1) {
                  print( "Keeping: ")
                  print( spi [ iip [tokeep] , ] )
                  spi$todrop[ iip[-tokeep] ] = TRUE
                } else {
                  print ("Keeping: First evaluation")
                  spi$todrop[ iip[ setdiff( 1:length(iip), 1) ] ] = TRUE
                }
              }
            }
          }
          todrop = which( spi$todrop )
          if (length(todrop) > 0 ) spi = spi[ -todrop, ]
        }
      }
      spi$todrop = NULL


      # fill in missing names, etc
      i = which(is.na( spi$name.scientific))
      if (length(i) > 0 ) spi$name.scientific[i] = lookup.tsn2taxa( tsn=spi$itis.tsn[i], vn="sci" )

      
      i = which(is.na( spi$name.common))
      if (length(i) > 0 ) spi$name.common[i] = lookup.tsn2taxa( tsn=spi$itis.tsn[i], vn="tx" )
        # have to do it again and fill with scientific name if missing
        i = which(is.na( spi$name.common))
        if (length(i) > 0 ) spi$name.common[i] = lookup.tsn2taxa( tsn=spi$itis.tsn[i], vn="sci" )


      i = which(is.na( spi$name.common.bio))
      if (length(i) > 0 ) spi$name.common.bio[i] = lookup.tsn2taxa( tsn=spi$itis.tsn[i], vn="tx" )
        # have to do it again and fill with scientific name if missing
        i = which(is.na( spi$name.common.bio))
        if (length(i) > 0 ) spi$name.common.bio[i] = lookup.tsn2taxa( tsn=spi$itis.tsn[i], vn="sci" )



  		# make sure the remainder of missing spec.clean points to spec
			i = which( !is.finite(spi$spec.clean) )
			if (length(i)>0) spi$spec.clean[i] = spi$spec[i]
    
      
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
				print( "gstaxa_taxonomy.csv -- see spcodes.itis.redo, above .. these are stored in with '|' as delimiter " ) 
				print( spi[i,] )
				fn2 = file.path( taxadir, "spcodes.no.itis.matches.csv" )
				print (fn2 )
				write.csv ( spi[i,], file=fn2 )
			}

      save( spi, file=fn, compress=T )

			# find.parsimonious.spec = T
			if (find.parsimonious.spec) {
				ii = which( spi$tolookup )
        # find most parsimonious list of species and place into spec.clean
				spi$spec.parsimonius[ii] = taxa.specid.correct( spi$spec[ii] )
				psm = which(spi$spec.parsimonius != spi$spec.clean )
				if (length(psm)>0) spi$spec.clean[psm] = spi$spec.parsimonius[psm]

				spi$spec.parsimonius2[ii] = taxa.specid.correct( spi$spec.clean[ii] )
				psm2 = which(spi$spec.parsimonius2 != spi$spec.clean)
				if (length(psm2)>0) spi$spec.clean[psm2] = spi$spec.parsimonius2[psm2]
			  
        save( spi, file=fnp, compress=T )
			}

      return ( fn )
    }

    # ------------------

    if (DS %in% c("full.taxonomy", "full.taxonomy.redo") ) {
      
      # add full taxonomic hierarchy to spcodes database .. 
	
      require ( multicore ) # simple parallel interface (using threads)
		
      itis.taxa.lowest = tolower(itis.taxa.lowest)
      fn = file.path( taxadir, paste("spcodes", itis.taxa.lowest, "rdata", sep=".") )
      
      if (DS=="full.taxonomy") {
        load(fn)
        return(spf)
      }

      spf = taxa.db( "spcodes.itis" )
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

			res = mclapply( 1:nrow(spf), itis.format, tsn=spf$itis.tsn, itaxa=itaxa, tunits=tunits )
      res = unlist( res) 
      res = as.data.frame( matrix( res, nrow=nrow(spf), ncol=length(formatted.names), byrow=T ), stringsAsFactors=F )
      colnames(res) = tolower(formatted.names)

      spf = cbind( spf, res )

      save( spf, file=fn, compress=T )
      return ( fn )
    
    }

    if (DS %in% c( "life.history", "life.history.redo") ) {
      
      fn = file.path( taxadir, "spcodes.lifehistory.rdata") 
      fn.local = file.path( taxadir, "gstaxa_working.csv") 
      
      if (DS == "life.history" ) {
        load(fn)
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

      sps = taxa.db( DS="full.taxonomy", itis.taxa.lowest="species" )
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
		  fn = file.path( taxadir, "spcodes.complete.rdata") 
  	  sps = NULL
			if (DS == "complete" ) {
        if (file.exists(fn)) load(fn)
        return(sps)
      }

			sp = NULL

			tx = taxa.db("life.history")
			# tx = tx[, c("spec", "spec.clean", "itis.tsn", "rank", "rank_id", "tsn.hierarchy")]  
			# spec.clean contains manually updated spec id's
			# this routine will update these by starting with the itis tsn hierarchy
		
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
			return (fn)
		} 
		

    # ----------------------------------------------
    
    if (DS %in% c( "itis.oracle", "itis.oracle.redo" ) ) {
      
      ### NOT USED ??? TO DELETE ?
      
      fn.itis = file.path( taxadir, "itis.oracle.rdata" )
      if (DS=="itis.oracle" ) {  
        load( fn.itis )
        return (itis)
      }
      itis.groundfish = taxa.db( "itis.groundfish.redo" )
      itis.observer = taxa.db( "itis.observer.redo" )
      toextract = colnames( itis.observer)  # remove a few unuses vars
      itis = rbind( itis.groundfish[,toextract] , itis.observer[,toextract] )
      ii = duplicates.toremove( itis$given_spec_code )
      itis = itis[ -ii, ]
      save(itis, file=fn.itis, compress=T)
      return (itis)
    }
    
    if (DS %in% c( "itis.groundfish", "itis.groundfish.redo" ) ) {
      
      
      ### NOT USED ??? TO DELETE ?
      
      fn.itis = file.path( taxadir, "itis.groundfish.rdata" )
      if (DS=="itis.groundfish" ) {  
        load( fn.itis )
        return (itis)
      }
      require(RODBC)
      connect=odbcConnect( oracle.taxonomy.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
      itis = sqlQuery(connect, "select * from groundfish.itis_gs_taxon")
      odbcClose(connect)
      names(itis) =  tolower( names( itis ) )
      for (i in names(itis) ) itis[,i] = as.character( itis[,i] )
      save(itis, file=fn.itis, compress=T)
      return (itis)
    }
    
    if (DS %in% c( "itis.observer", "itis.observer.redo" ) ) {
      
      
      ### NOT USED ??? TO DELETE ?
      
      fn.itis = file.path( taxadir, "itis.observer.rdata" )
      if (DS=="itis.observer" ) {  
        load( fn.itis )
        return (itis)
      }
      require(RODBC)
      connect=odbcConnect( oracle.taxonomy.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
      itis = sqlQuery(connect, "select * from observer.itis_isdb_species")
      odbcClose(connect)
      names(itis) =  tolower( names( itis ) )
      for (i in names(itis) ) itis[,i] = as.character( itis[,i] )
      save(itis, file=fn.itis, compress=T)
      return (itis)
    }

  }


