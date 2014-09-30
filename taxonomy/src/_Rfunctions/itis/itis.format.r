

  itis.format = function( i, tsn, itaxa, tunits ) { 

		res = c( i, rep(NA, nrow(tunits)+5) )
		if ( !is.finite( tsn[i] )) return (res)

    o = itis.traverse( tsn[i], itaxa )
		if ( is.null(o) ) return(res)  # "vernacular" and "possible.error", etc are added below
	  if ( min(o$rank_id) > max(tunits$rank_id)) { 
			# likely an error
			return(res)
		}

		tsn.hierarchy = paste( o$tsn, collapse="~")
			
    ot = merge( tunits, o, by=c("rank_id"), all.x=T, all.y=F, sort=T )
    oti = which ( is.finite( ot$tsn ) )
    otj = which ( !is.finite( ot$tsn ) )

    rank_id_lowest = max ( ot$rank_id[oti] )
    ol = which( ot$rank_id==rank_id_lowest )

    real.na = which( ot$rank_id > rank_id_lowest )
 
    # these NA's are simply due to taxonomies not being complete or resolved 
    # convert to "" as defualt as NA is reserved for taxalevels that should not be resolved
   
    otj = setdiff( otj, real.na ) 
    ot$unit_name1[ otj ] = ""
    res = c( i, ot$unit_name1, ot$vernacular[ ol ], ot$possible.error[ ol ], rank_id_lowest,
						 tunits$rank_name[which(tunits$rank_id==rank_id_lowest) ], tsn.hierarchy )
    names(res) = c( "rowindex", ot$rank_name, "vernacular", "possible.error", "rank_id", "rank", "tsn.hierarchy")
    return (res)
  }

  
