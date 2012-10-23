
	specieslist.parsimony = function( DS="default" ) {
		
		# take BIO species codes and determine recodes based upon taxonomic databases such as ITIS, etc.
    fn = file.path( project.directory("taxonomy"), "data", paste( "spcodes.itis", DS, "rdata", sep=".") )
	  sp = NULL
		if (DS == "default" ) {
      if (file.exists(fn)) load(fn)
      return(sp)
    }

		sp = taxa.db("complete")
		
		ranks = sort( unique( sp$rank_id ),decreasing=T )
		ranks = setdiff( ranks, 220 )  # 220 is the lowest level of discrimination (species)
		
		# search for single children and a parent, recode parent to child
		for ( r in ranks ) {
			oo = which( sp$rank_id == r )
			for ( o in oo ) {
				if ( is.finite(sp$children.n[o]) && sp$children.n[o] == 1) {
					# only child --> recode this spec to child's spec 
					newspec = which( sp$itis.tsn == sp$children[o] ) # multiple matches likely as this is a recursive process -- pick lowest taxa level == highest rank_id
					nsp = newspec[which.max( sp$rank_id[newspec] )]
					sp$spec.clean[o] = sp$spec.clean[nsp]
					sp$children[o] = sp$children[nsp]
					sp$children.n[o] = sp$children.n[nsp]
					print( paste(  "Updating species list::", sp$spec.clean[o], sp$sci[o], "->", sp$spec.clean[nsp], sp$sci[nsp] ) )
				}
			}
		}

		save (sp, file=fn, compress=T) 
		return(fn)
  }


