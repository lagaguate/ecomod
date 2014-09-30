


  itis.traverse = function( tsn, itaxa ) {
     
    out = itis.extract ( tsn, itaxa  )
    if (length(out) %in% c(0,1) ) {
			return( NULL )  # return NULL
    }

    i = 1
    repeat { 
      r0 = itis.extract ( out$parent_tsn[i], itaxa )
      if ( length (r0) %in% c(0,1) ) break()
      out = rbind( out, r0 )
      i = i + 1
      # print (i)
      if ( out$parent_tsn[i] == 0 ) break ()
    }

    u = which( out$rank_id == 230 ) 
    out$unit_name1[u] = out$unit_name3[u]  # subspecies 
    
    u = which( out$rank_id == 220 ) 
    out$unit_name1[u] = out$unit_name2[u]  # species 
    out = out[ , c("tsn", "kingdom_id", "rank_id", "unit_name1", "vernacular", "name_usage", "possible.error" )]
    
    return (out )

  }


