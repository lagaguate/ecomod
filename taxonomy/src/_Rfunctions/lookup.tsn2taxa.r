

  lookup.tsn2taxa = function( tsn, tx=itis.db() ) { 
    out = data.frame( tsn=tsn )
    out$tx = NA
		out$sci = NA
		out$usage = NA
    for ( i in 1:length(tsn) ) {
      itx = which( tx$tsn == tsn[i] )
      if (length(itx) == 1 ) {
	      out$sci[i] = tx$completename[itx]
		    out$tx[i] = tx$vernacular_name[itx]
				out$usage[i] = tx$name_usage[itx]
				out$rank[i] = tx$rank_id[itx]
				out$parent[i] = tx$parent_tsn[itx]
			}
    }
    return (out) 
  }



