
  itis.db = function( DS="itaxa", lnk=NULL, itis.kingdom=NULL ) {
    
		datdir = file.path( project.directory("taxonomy"), "data" )
    itis.dir = file.path( project.directory("taxonomy"), "itis" )

    dir.create( datdir, showWarnings = FALSE, recursive = TRUE )

    if (DS=="make.snapshot") {
      # unix-specific commands .. make more generic
      system( paste( "rm -rf", itis.dir ) )
      dir.create( itis.dir, showWarnings = FALSE, recursive = TRUE )
      if (is.null(lnk)) lnk = "http://www.itis.gov/downloads/itisMySqlTables.tar.gz"
      fn = file.path( tempdir(), basename(lnk) ) 
      download.file( url=lnk, destfile=fn )
      system( paste( "tar -zxv --strip-components=1 --directory=", itis.dir, " --file=", fn, sep="" ) ) 
      system( paste( "gzip -r", itis.dir ) )
      return ( itis.dir )
    }


    if ( DS %in% c( "main.redo", "taxon.unit.types", "kingdoms", "itaxa" ) ) {

      fn.itaxa = file.path( datdir, "itis.itaxa.rdata" )
      fn.taxon.unit.types = file.path( datdir, "itis.taxon.units.rdata" )
      fn.kingdoms = file.path( datdir, "itis.kingdoms.rdata" )
      
      if (DS=="kingdoms") {
        load( fn.kingdoms )
        return ( kingdoms )
      }

      if (DS=="taxon.unit.types") {
        load( fn.taxon.unit.types )
				if ( is.null(itis.kingdom) || itis.kingdom=="all" ) {
					return (taxon.unit.types)
				}
				kingdoms = itis.db( "kingdoms" )
				kid = kingdoms$kingdom_id[ which( tolower(kingdoms$kingdom_name)==tolower(itis.kingdom) ) ]
				taxon.unit.types = taxon.unit.types[ which( taxon.unit.types$kingdom_id == kid ) , ]
				return (taxon.unit.types)
      }
     
      if (DS=="itaxa") {
        load( fn.itaxa )
				if ( is.null(itis.kingdom) || itis.kingdom=="all"  ) {
					return ( itaxa )
				}
				kingdoms = itis.db( "kingdoms" )
				kid = kingdoms$kingdom_id[ which( tolower(kingdoms$kingdom_name) %in% tolower(itis.kingdom) ) ]
				itaxa = itaxa[ which( itaxa$kingdom_id %in% kid ) ,]
				return ( itaxa )
      }

      # the use of apostrophes (') confuses the reading as it is a quotation too. quote="" solves this.
      taxonomic.units = read.csv( gzfile( file.path( itis.dir, "taxonomic_units.gz" ) ), sep="|", stringsAsFactors=F, quote="", fill=T, header=F )
      names( taxonomic.units ) = c( "tsn", "unit_ind1", "unit_name1", "unit_ind2", "unit_name2", "unit_ind3", "unit_name3", "unit_ind4", "unit_name4", "unnamed_taxon_ind", "name_usage", "unaccept_reason", "credibility_rtng", "completeness_rtng", "currency_rating", "phylo_sort_seq", "initial_time_stamp", "parent_tsn", "taxon_author_id", "hybrid_author_id", "kingdom_id", "rank_id", "update_date", "uncertain_prnt_ind" )
      # primary  keys = tsn
      taxonomic.units = taxonomic.units[, c( "tsn", "name_usage", "parent_tsn", "kingdom_id", "rank_id", "unit_name1", "unit_name2", "unit_name3", "unit_name4" )]
      taxonomic.units$unit_name1 = iconv( taxonomic.units$unit_name1, from="latin1", to="UTF-8")
      taxonomic.units$unit_name2 = iconv( taxonomic.units$unit_name2, from="latin1", to="UTF-8")
      taxonomic.units$unit_name3 = iconv( taxonomic.units$unit_name3, from="latin1", to="UTF-8")
      taxonomic.units$unit_name4 = iconv( taxonomic.units$unit_name4, from="latin1", to="UTF-8")
      n0 = nrow( taxonomic.units )
   

      geographic.div = read.csv( gzfile( file.path( itis.dir, "geographic_div.gz" ) ), sep="|", stringsAsFactors=F, quote="", fill=T , header=F)
      names( geographic.div ) = c( "tsn", "geographic_value", "update_date" )
      # primary keys = tsn, geographic_value
      geographic.div = geographic.div[, c( "tsn", "geographic_value")]
        G = data.frame( tsn = unique( geographic.div$tsn ) )
        G = merge( G, geographic.div, by=c("tsn"), all.x=T, all.y=F, sort=F )
        i = which(duplicated( G$tsn) )
        j = unique( G$tsn[i] )
        ntot = length (j )
        for ( ii in 1:length(j) ) {   ###<<<<<<<<<< TODO: make this parallel with mcapply

          print( paste( ii, "of", ntot  ) )
          k = which( G$tsn == j[ii] ) 
          G$geographic_value[ k[1] ] = paste( G$geographic_value[ k ], collapse=" + " )
        }
        geographic.div = G[ -i , ]  # remove the duplicated records
        rm (G)

      
      itaxa = merge( taxonomic.units, geographic.div, by="tsn", all=T, sort=F )
      if ( nrow(itaxa) != n0 ) stop("Merge Error ")


      jurisdiction = read.csv( gzfile( file.path( itis.dir, "jurisdiction.gz" ) ), sep="|", stringsAsFactors=F, quote="", fill=T, header=F )
      names( jurisdiction ) = c( "tsn", "jurisdiction_value", "origin", "update_date" )
      # primary keys = jurisdiction_index (tsn, jurisdiction_value)
      jurisdiction = jurisdiction[, c( "tsn", "jurisdiction_value", "origin") ]
        G = data.frame( tsn = unique( jurisdiction$tsn ) )
        G = merge( G, jurisdiction, by=c("tsn"), all.x=T, all.y=F, sort=F )
        G$jurisdiction = NA
        G$juri.tmp = paste( G$jurisdiction_value, G$origin )
        i = which(duplicated( G$tsn) )
        j = unique( G$tsn[i] )
        ntot = length (j )
        for ( ii in 1:length(j) ) {   ###<<<<<<<<<< TODO: make this parallel with mcapply

          print( paste( ii, "of", ntot  ) )
          k = which( G$tsn == j[ii] ) 
          G$jurisdiction[ k[1] ] = paste( G$juri.tmp[ k ], collapse=" + " )
        }
        jurisdiction = G[ -i , c("tsn", "jurisdiction" ) ]  # remove the duplicated records
        rm (G)

      itaxa = merge( itaxa, jurisdiction, by="tsn", all=T, sort=F )
      if ( nrow(itaxa) != n0 ) stop("Merge Error ")


      longnames = read.csv( gzfile( file.path( itis.dir, "longnames.gz" ) ), sep="|", stringsAsFactors=F, quote="", fill=T, header=F )
      names( longnames ) = c( "tsn", "completename" )
      longnames$completename = iconv( longnames$completename, from="latin1", to="UTF-8")
      # primary keys = tsn
      
      i = which(duplicated( longnames$tsn))
      if (length (i) > 0 ) stop( "Error: longnames has duplicates" )

      itaxa = merge( itaxa, longnames, by="tsn", all=T, sort=F )
      if ( nrow(itaxa) != n0 ) stop("Merge Error ")


      synonym.links = read.csv( gzfile( file.path( itis.dir, "synonym_links.gz" ) ), sep="|", stringsAsFactors=F, quote="", fill=T, header=F )
      names( synonym.links ) = c( "tsn", "tsn_accepted", "update_date" )
      # primary keys = tsn
      synonym.links = synonym.links[ ,  c( "tsn", "tsn_accepted") ]
        G = data.frame( tsn = unique( synonym.links$tsn ) )
        G = merge( G, synonym.links, by=c("tsn"), all.x=T, all.y=F, sort=F )
        i = which( duplicated( G$tsn) )
        j = unique( G$tsn[i] )
        ntot = length (j )
        for ( ii in 1:length(j) ) {   ###<<<<<<<<<< TODO: make this parallel with mcapply

          print( paste( ii, "of", ntot  ) )
          k = which( G$tsn == j[ii] ) 
          G$tsn[ k[1] ] = paste( G$tsn[ k ], collapse=" + " )
        }
        synonym.links = G[ -i , ]  # remove the duplicated records
        rm (G)

      # there exist other tsn's in synonyms not found in main table
      itaxa = merge( itaxa, synonym.links, by="tsn", all.x=T, all.y=F, sort=F )   
      if ( nrow(itaxa) != n0 ) stop("Merge Error ")


      vernaculars =  read.csv( gzfile( file.path( itis.dir, "vernaculars.gz" ) ), sep="|", stringsAsFactors=F, quote="", fill=T, header=F )
      names( vernaculars ) = c( "tsn", "vernacular_name", "language", "approved_ind", "update_date", "vern_id" )


      # primary keys = tsn, vern_id
      # multiple names exist for many tsn's  
      vernaculars = vernaculars[  c( "tsn", "vernacular_name", "language" ) ]  # choose english where possible
      vernaculars$vernacular_name = iconv( vernaculars$vernacular_name, from="latin1", to="UTF-8" ) 
        V = data.frame( tsn = unique( vernaculars$tsn ) )
        V = merge( V, vernaculars, by=c("tsn"), all.x=T, all.y=F, sort=F )
        V$vernacular = V$vernacular_name
        i = which(duplicated( V$tsn) )
        j = unique( V$tsn[i] )
        ntot = length (j )
        for ( ii in 1:length(j) ) {   ###<<<<<<<<<< TODO: make this parallel with mcapply
          k = which( V$tsn == j[ii] ) 
          print( paste( ii, "of", ntot ) )
          # print( paste( ii, "of", ntot, ":::" , paste( V$vernacular_name[ k ], collapse=" + ")   ) )
          oo = k[ which( V$language[k] %in% c("English", "unspecified") ) ]
          pp = ifelse (length(oo) > 0,  oo[1], k[1] )
          VV = V$vernacular_name[ pp ]  # temporary storage until the rest gets updated
          V$vernacular_name[ k[1] ] = paste( V$vernacular_name[ k ], collapse=" + " )
          V$vernacular[ k[1] ] = VV 
          # print( VV )
        }
        vernaculars = V[ -i , ]  # remove the duplicated records
        rm (V)

      itaxa = merge( itaxa, vernaculars, by="tsn", all.x=T, all.y=F, sort=F )   
      if ( nrow(itaxa) != n0 ) stop("Merge Error ")

      kingdoms = read.csv( gzfile( file.path( itis.dir, "kingdoms.gz" ) ), sep="|", stringsAsFactors=F, quote="", fill=T, header=F )
      names( kingdoms ) = c( "kingdom_id", "kingdom_name", "update_date" )
      # primary keys = kingdom_id
      kingdoms = kingdoms[ , c( "kingdom_id", "kingdom_name" ) ]
 

      taxon.unit.types = read.csv( gzfile( file.path( itis.dir, "taxon_unit_types.gz" ) ), sep="|", stringsAsFactors=F, quote="", fill=T, header=F )
      names( taxon.unit.types ) = c( "kingdom_id", "rank_id", "rank_name" , "dir_parent_rank_id", "req_parent_rank_id", "update_date" ) 
      # primary keys = kingdom_id, rank_id
      taxon.unit.types = taxon.unit.types[ , c( "kingdom_id", "rank_id", "rank_name" , "dir_parent_rank_id", "req_parent_rank_id" ) ]


      save( itaxa, file=fn.itaxa, compress=T )
      save( kingdoms, file=fn.kingdoms, compress=T )
      save( taxon.unit.types, file=fn.taxon.unit.types, compress=T )

      return ( "Done" )


      ### the rest are unused
        just.what.is.needed = T
        if (! just.what.is.needed) {
          # additional tables ... not really needed
          
          hierachy = read.csv( gzfile( file.path( itis.dir, "hierarchy.gz" ) ), sep="|", stringsAsFactors=F, quote="", fill=T, header=F )
          names( hierachy ) = c( "hierarchy_string" )
          # primary keys = hierarchy_string 
          # these are tsn values delimited by a hyphen from parent-child

          comments = read.csv( gzfile( file.path( itis.dir, "comments.gz" ) ), sep="|", stringsAsFactors=F, quote="", fill=T, header=F )
          names( comments ) = c( "comment_id", "commentator", "comment_detail", "comment_time_stamp", "update_date" )
          # primary keys = comment_id

          taxa_comments_links = read.csv( gzfile( file.path( itis.dir, "tu_comments_links.gz" ) ), sep="|", stringsAsFactors=F, quote="", fill=T, header=F )
          names( taxa_comments_links ) = c( "tsn", "comment_id", "update_date" )
          # primary keys = tsn, comment_id
        }


    }
  
  }


