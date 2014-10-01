
  taxonomy.recode = function( from=NULL, to=NULL, tolookup=NULL, outvars=NULL, spi=NULL ){
    
    # various methods for looking up taxa or species id numbers ..

    if (from %in% c("internalcodes", "spec_bio", "spec.parsimonious", "parsimonious" ) ) {
      
      out = NULL
      spi = taxonomy.db( "parsimonious" )
      
      # duplicates will be created as spec.parsimonious is not "unique"

      if (is.null(to)) {   # i.e., default behaviour 
        out = data.frame( spec.parsimonious=tolookup, sortorder=1:length(tolookup), tsn=NA, tx=NA, vern=NA, othertaxacodes=NA  )
        for ( i in 1:length( tolookup ) ) {
          itx = which( spi$spec.parsimonious == tolookup[i] )
          if (length(itx) == 0 ) next()
          out$tx_index[i] = itx[1]
          out$othertaxacodes[i] = paste(itx, collapse="; ")
        }
        good = which( is.finite( out$tx_index) )
        good_index = out$tx_index[good]
        out$tx[ good ]   = spi$name.scientific[good_index ]
        out$vern[ good ] = spi$name.common[ good_index ]
        out$tsn[ good ]  = spi$itis.tsn[ good_index ]
        out = out[ order( out$sortorder) , ]
        out$sortorder = NULL
        if (!is.null( outvars )) {  # if more than 1 variable
          out = out[ , outvars] 
        }
        return (out) 

      } else {

        if ( to %in% c( "spec", "groundfishcodes" ) ) {
          # take BIO species codes and determine new codes based upon taxonomic databases such as ITIS, etc.
          out = data.frame( spec.parsimonious=tolookup, sortorder=1:length(tolookup) )
          out = merge( out, spi [, c("spec", "spec.parsimonious")], by="spec.parsimonious", all.x=T, all.y=F, sort=F )
          od = which( duplicated( out$sortorder) ) 
          if (length(od)>1) out = out[ - od, ]  # only return the first element ..
          out = out[ order( out$sortorder) , ]
          out$sortorder = NULL
          return( out$spec )  
        }
        
        # else default 
        
        out = data.frame( spec.parsimonious=tolookup, sortorder=1:length(tolookup), tsn=NA, tx=NA, vern=NA , othertaxacodes=NA  )
        for ( i in 1:length( tolookup ) ) {
          itx = which( spi$spec.parsimonious == tolookup[i] )
          if (length(itx) == 0 ) next()
          out$tx_index[i] = itx[1]
          out$othertaxacodes[i] = paste(itx, collapse="; ")
        }
        good = which( is.finite( out$tx_index) )
        good_index = out$tx_index[good]
        out$tx[ good ]   = spi$name.scientific[good_index ]
        out$vern[ good ] = spi$name.common[ good_index ]
        out$tsn[ good ]  = spi$itis.tsn[ good_index ]
        out = out[ order( out$sortorder) , ]
        out$sortorder = NULL
        if (exists( to, out) ) {
          return( out[,to])
        }
        if (!is.null( outvars )) {  # if more than 1 variable
          out = out[ , outvars] 
          return (out) 
        } else {
          return (out)
        }
      
      } 
    
    }

    
    # -----------------

 
    if (from %in% c("spec", "groundfishcodes" ) ) {
      
      out = NULL
      spi = taxonomy.db( "parsimonious" )

      if (is.null(to)) {   # i.e., default behaviour 
        out = data.frame( spec=tolookup, sortorder=1:length(tolookup), tsn=NA, tx=NA, vern=NA  )
        for ( i in 1:length( tolookup ) ) {
          itx = which( spi$spec == tolookup[i] )
          if (length(itx) == 0 ) next()
          out$tx_index[i] = itx[1]
        }
        good = which( is.finite( out$tx_index) )
        good_index = out$tx_index[good]
        out$tx[ good ]   = spi$name.scientific[good_index ]
        out$vern[ good ] = spi$name.common[ good_index ]
        out$tsn[ good ]  = spi$itis.tsn[ good_index ]
        out = out[ order( out$sortorder) , ]
        out$sortorder = NULL
        if (!is.null( outvars )) {  # if more than 1 variable
          out = out[ , outvars] 
        }
        return (out) 

      } else {

        if ( to=="parsimonious" ) {
          # take BIO species codes and determine new codes based upon taxonomic databases such as ITIS, etc.
          out = data.frame( spec=tolookup, sortorder=1:length(tolookup) )
          out = merge( out, spi [, c("spec", "spec.parsimonious")], by="spec", all.x=T, all.y=F, sort=F )
          out = out[ order( out$sortorder) , ]
          out$sortorder = NULL
          return( out$spec.parsimonious )  
        }
        
        # else default 
        
        out = data.frame( spec=tolookup, sortorder=1:length(tolookup), tsn=NA, tx=NA, vern=NA  )
        for ( i in 1:length( tolookup ) ) {
          itx = which( spi$spec == tolookup[i] )
          if (length(itx) == 0 ) next()
          out$tx_index[i] = itx[1]
        }
        good = which( is.finite( out$tx_index) )
        good_index = out$tx_index[good]
        out$tx[ good ]   = spi$name.scientific[good_index ]
        out$vern[ good ] = spi$name.common[ good_index ]
        out$tsn[ good ]  = spi$itis.tsn[ good_index ]
        out = out[ order( out$sortorder) , ]
        out$sortorder = NULL
        if (exists( to, out) ) {
          return( out[,to])
        }
        if (!is.null( outvars )) {  # if more than 1 variable
          out = out[ , outvars] 
          return (out) 
        } else {
          return (out)
        }
      
      } 
    
    }

    
    # -----------------

 

    if ( from %in% c("taxa.fast", "taxa.local.lookup" ) ) {
       
      if( is.null(spi)) spi = taxonomy.db("parsimonious") 
      if (is.null(to)) {   # i.e., default behaviour 
        v = as.vector(unlist( strsplit( tolookup, "[[:space:]]+", fixed=FALSE, perl=FALSE, useBytes=FALSE) ))
        out = list()
        for (i in 1:length(v)) {
          w = paste("\\<", v[i], "\\>", sep="")
          sn  = grep( w, spi$name.scientific, ignore.case=T )
          vn  = grep( w, spi$name.common, ignore.case=T )
          vn2 = grep( w, spi$name.common.worktable, ignore.case=T )
          vn3 = grep( w, spi$vernacular, ignore.case=T )
          out[[i]] = sort( unique( c(sn, vn, vn2, vn3)  ))
        }
        if (length(out)==0) {
          sp = -1  # no match
        } else  {
          sp = spi$spec[out[[1]]]
        }
        sp2 = sp
        if (length(out) > 1) {
          for ( i in 2:length(v)) {
            sp2 = intersect( sp2, out[[i]] )
          }
        }
        if (length(sp2)==1) { 
          sp=sp2 
        } else { 
          sp = sort( unique( c( sp, sp2 ) ) )
        }
        return(sp)
      }

    } 
   

    # -----------------
    

    if( from %in% c(  "taxa", "name.common", "name.scientific", "vernacular" ) )  {
    
      out = list()
      itaxa = itis.db()
      spi = taxonomy.db("parsimonious") ## loading here reduces disk re-read overhead

      for ( i in 1:length( tolookup ) ) {

        word = tolookup[i]
        word = taxonomy.strip.unnecessary.characters( word )

        tsn0 = tsn1 = NULL

        if (from %in% c( "taxa", "name.scientific" ) ) tsn0 = itis.taxa.to.tsn( tx=word, itaxa=itaxa )
        if (from %in% c( "taxa",  "name.common", "vernacular" ) ) tsn1 = itis.vernacular.to.tsn(  tx=word, itaxa=itaxa )
        tsn = sort( unique( c(tsn0, tsn1))) 

        spec= taxonomy.recode( from="taxa.fast", tolookup=word, spi=spi )

        # cross-reference itis and taxonomy.db id's
        if ( length(spec) > 0 ) {
          tsn.add = spi$itis.tsn[ which( spi$spec %in% spec) ]
          tsn = sort( unique( c( tsn, tsn.add) ) )
        } 
        
        if ( length(tsn) > 0 ) {
          spec.add = spi$spec [which( spi$itis.tsn %in% tsn) ]
          spec = sort( unique( c( spec, spec.add) ) )
        }

        out[[i]] = list(taxa=tolookup[i], tsn=tsn, spec=spec )

      }
      return (out) 
    }



    if ( from =="taxa.dirty") {  
      
      # 1. clean up taxa information 
      # 2. lookup itis.tsn 
      # 3. using itis.tsn and then reverse lookup of spec 
      # 4. then create spec if not found
   
      out = data.frame( taxa=tolookup, sortorder=1:length(tolookup), spec=NA, itis.tsn=NA, tolookup=TRUE, flag="", stringsAsFactors=F )
     
      qq = which( is.na( out$taxa ) )
      if (length( qq) > 0 ) {
        out$tolookup[qq] = FALSE
        out$flag[qq] = "No useful data"
      }

      out = taxonomy.keywords.flag( out, "taxa" )
      out = taxonomy.keywords.flag( out, "taxa", wds=c(
        "perhaps", "empty shells", "unknown", "pink lump", "frozen", "shells scal", 
        "unidentified", "saved for identification", "unident", "saved for id", "maybe", "arc", 
        "unid", "invert unsp"  ) )

      # singleton characters
      out$taxa = gsub( "\\<[[:alnum:]]{1}\\>", " ", out$taxa , ignore.case=T )  # remove ()/, etc.

      out = taxonomy.keywords.remove( out, "taxa", withpunctuation=T )
      out = taxonomy.keywords.remove( out, "taxa", withpunctuation=F )
      
      out = taxonomy.keywords.remove( out, "taxa", withpunctuation=F, wds.toremove=c(
        "no", "new", "not as before", "frozennnnn" ) )
      
      out$taxa = taxonomy.strip.unnecessary.characters( out$taxa )
      
      vnames = c( "taxa", "taxa" )
      vtypes = c( "default", "vernacular" )
      out = itis.lookup.exhaustive.search( out, vnames, vtypes )

      out$spec = taxonomy.recode (from="tsn", to="spec", tolookup=out$itis.tsn )
      ww = which( !is.finite( out$spec ) )
      if (length(ww)>0) out$spec[ww] = - out$itis.tsn[ww]  ## take the negative value of the itis tsn
   
      out = out[ order( out$sortorder) , ]
      out$sortorder = NULL
       
      return (out)
    }


    if ( from %in% c( "tsn", "itis")  ) {  
    
      if (to %in% c("taxa", "taxa.itis", "sci", "spi", "usage", "rank", "parent" ) ) {  
        ### lookup is from itis database
        out = data.frame( tsn=tolookup, sortorder=1:length(tolookup) )
        tx=itis.db() 
        tx = tx[, c("tsn", "completename", "vernacular_name", "name_usage", "rank_id", "parent_tsn" ) ]
        out = merge (out, tx, by="tsn", all.x=T, all.y=F, sort=F )
        out = out[ order( out$sortorder) , ]
        out$sortorder = NULL
        names(out) = c("tsn", "sci", "tx", "usage", "rank", "parent" ) 
             
        if ( length(to)==1 ){
          if (exists( to, out) ) return( out[,to])
        }
        return (out) 
      } 
      
      if (to %in% c("spec", "name" )) {  
        ### lookup is from the taxa database
        spi = taxonomy.db( "parsimonious" )
        out = data.frame( tsn=tolookup, spec=NA, name=NA, sortorder=1:length(tolookup) )
        for ( i in 1:length(tolookup) ) {
          itx = which( spi$itis.tsn == tolookup[i] )
          if (length(itx) == 1 && spi$usage[itx] %in% c("accepted", "valid") ) {
            out$spec[i] = spi$spec[itx]
            out$name[i] = spi$name.scientific[itx] # for debugging
          }
        }
        out = out[ order( out$sortorder) , ]
        out$sortorder = NULL
       
        if ( length(to) ==1 ){
          if (exists( to, out) ) return( out[,to])
        }
        return (out)
      }
    
      if ( to=="parsimonious" ) {
        # take BIO species codes and determine new codes based upon taxonomic databases such as ITIS, etc.
        out = data.frame( spec=spec, sortorder=1:length(spec) )
        spi = taxonomy.db( "parsimonious" )
        out = merge( out, spi [, c("tsn", "spec.parsimonious")], by="tsn", all.x=T, all.y=F, sort=F )
        out = out[ order( out$sortorder) , ]
        return( out$spec.parsimonious )  
      }


    
    }
 
  
  }



