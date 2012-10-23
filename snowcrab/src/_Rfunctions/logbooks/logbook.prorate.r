
  logbook.prorate = function(logbook) {
    
    # manual pro-rate:
    names(logbook) = tolower( names(logbook) )
  
    # fix problems with slip weights
      new.slip.sums = tapply2( x=logbook, indices=c("doc_id"), var="slip_weight_lbs", newvars=c("new.slip.wgt"),
                       func=function(x){ sum(unique(x)) } )

      logbook = merge( x=logbook, y=new.slip.sums, by="doc_id", sort=F, all.x=T, all.y=F)
      i = which(( logbook$new.slip.wgt - logbook$slip_weight_lbs) != 0 )
      j = which( duplicated( logbook$log_efrt_std_info_id,  ))
      bad.data = intersect ( i, j )
      logbook = logbook[-bad.data ,]

      logbook$slip_weight_lbs = logbook$new.slip.wgt
      logbook$new.slip.wgt = NULL

      # counts
      pr.count = tapply2( x=logbook, indices=c("doc_id"), var="log_efrt_std_info_id", newvars=c("pr.count"),
                       func=function(x){ length(unique(x)) } )
      # sums
      pr.sum = tapply2( x=logbook, indices=c("doc_id"), var="est_weight_log_lbs", newvars=c("pr.sum"),
                       func=function(x){ sum(x, na.rm=T) } )

      # merge
      pr = merge(x=pr.count, y=pr.sum, bu="doc_id", sort=F, all=F)
      pr$doc_id = as.character(pr$doc_id )

      logbook = merge(x=logbook, y=pr, by="doc_id", all.x=T, all.y=F, sort=F)
      logbook$pr.fraction = logbook$est_weight_log_lbs / logbook$pr.sum
      logbook$pro_rated_slip_wt_lbs = logbook$pr.fraction * logbook$slip_weight_lbs

      # second pro-rating of data with missing values/nulls, etc
      na.logs = which (!is.finite ( logbook$pr.sum ) )
      logbook$pro_rated_slip_wt_lbs[ na.logs ] = logbook$slip_weight_lbs[na.logs ] / logbook$pr.count [  na.logs]

      bad.usage.codes = which ( logbook$catch_usage_code!=10 )
      logbook = logbook[-bad.usage.codes, ]

    return( logbook )
  }



