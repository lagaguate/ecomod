  lookup.taxa2tsn2spec = function( taxa  ) {
    
    # 1. clean up taxa information 
    # 2. lookup itis.tsn 
    # 3. using itis.tsn and then reverse lookup of spec 
    # 4. then create spec if not found
 
    res = data.frame( taxa=taxa, spec=NA, itis.tsn=NA, tolookup=TRUE, flag="", stringsAsFactors=F )
   
    qq = which( is.na( res$taxa ) )
    if (length( qq) > 0 ) {
      res$tolookup[qq] = FALSE
      res$flag[qq] = "No useful data"
    }

    res = taxonomy.flag.keywords( res, "taxa" )
    res = taxonomy.flag.keywords( res, "taxa", wds=c(
      "perhaps", "empty shells", "unknown", "pink lump", "frozen", "shells scal", 
      "unidentified", "saved for identification", "unident", "saved for id", "maybe", "arc", 
      "unid", "invert unsp"  ) )

    # singleton characters
    res$taxa = gsub( "\\<[[:alnum:]]{1}\\>", " ", res$taxa , ignore.case=T )  # remove ()/, etc.

    res = taxonomy.keywords.remove( res, "taxa", withpunctuation=T )
    res = taxonomy.keywords.remove( res, "taxa", withpunctuation=F )
    
    res = taxonomy.keywords.remove( res, "taxa", withpunctuation=F, wds.toremove=c(
      "no", "new", "not as before", "frozennnnn" ) )
    
    res$taxa = strip.unnecessary.characters(res$taxa )
    
    vnames = c( "taxa", "taxa" )
    vtypes = c( "default", "vernacular" )
    res = itis.lookup.exhaustive.search( res, vnames, vtypes )

    res$spec = lookup.tsn2spec( res$itis.tsn )
    ww = which( !is.finite( res$spec ) )
    if (length(ww)>0) res$spec[ww] = - res$itis.tsn[ww]  ## take the negative value of the itis tsn
    
    return (res)
  }
  

