

  lookup.tsn2taxa = function( tsn, tx=itis.db(), vn=NULL ) { 
    ### lookup is from itis database
    
    out = data.frame( tsn=tsn )
    tx=itis.db() [, c("tsn", "completename", "vernacular_name", "name_usage", "rank_id", "parent_tsn" ) ]

    out = merge (out, tx, by="tsn", all.x=T, all.y=F, sort=F )
    names(out) = c("tsn", "sci", "tx", "usage", "rank", "parent") 

    if (!is.null( vn)) out = out[, vn]

    return (out) 
  }



