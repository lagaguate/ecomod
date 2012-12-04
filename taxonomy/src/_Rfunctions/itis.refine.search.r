

itis.refine.search = function( out, itaxa, lower.taxa.limit=220 ) {

   res = NA
    if (length(out) > 0) {
      ss = itaxa[out,]
      
      ssv = which( ss$name_usage %in% c("valid", "accepted") )
      if ( length( ssv) > 0 ) {
        ss = ss[ ssv, ]
        isp = which( ss$rank_id <= 220 ) # accept to species level and higher (taxa-level) only
        if ( length(isp) == 1 ) {
          res = ss[ isp, ]
        }
      }
      
      # last try
      if ( is.null(nrow(res)) ) {
        tsnall = c( itaxa$tsn[out], itaxa$tsn_accepted[out], itaxa$parent_tsn[out] ) # add parent taxa in case
        tsnall = unique( tsnall[ is.finite( tsnall) ] )
        tsnall = tsnall[ which( tsnall > 0 ) ]

        ss = itaxa[ which(itaxa$tsn %in% tsnall ) ,]
        ssv = which( ss$name_usage %in% c("valid", "accepted") )
        if ( length( ssv) > 0 ) {
          ss = ss[ ssv, ]
          isp = which( ss$rank_id <= lower.taxa.limit ) # accept to (220) species level and higher (taxa-level) only
          if ( length(isp) > 0 ) {
            ss = ss[ isp, ]
            ismax = which.max( ss$rank_id)  # takes the first max .. even if there are others matching
            if ( length(ismax) == 1 ) {
              res = ss[ ismax, ]
            }     
          }
        }
      }
    }

  return (res)
}


