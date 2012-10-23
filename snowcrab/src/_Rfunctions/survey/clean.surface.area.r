

  clean.surface.area = function (set ) {
  

    #-- rerun netmind net metrics using new t0 t1 to determine sa

    require (boot)
      # final edits / sanity checks
      # many good tows where surface area was not recorded in the historical data
      # filter : set missing values and any large extremes to year-specific medians
      # historical data not touched except if missing

    years = sort(unique(set$yr))
    
    sa.all = mean( boot(set$sa, function(x,i) median( x[i], na.rm=T), R=1000 )$t )
    
    # up to this piint set$sa contains historical data, determined by "hand"
    # Merge with modern data streams
    # hard limits 
    qsah = c( 0.001, 0.01 ) # more extreme than these are really not possible -- force as errors
    qreject = c( 0.025, 0.975 ) # data extremes to accept/reject

    # historical data
    iqsah = which( set$sa < qsah[1] | set$sa > qsah[2] ) 
    if (length( iqsah)>0 )  set$sa[iqsah] = NA
    qsa = quantile( set$sa, qreject, na.rm=T )
    iqsa = which( set$sa < qsa[1] | set$sa > qsa[2] ) 
    if (length( iqsa)>0 )  set$sa[iqsa] = NA

    # modern data streams ...
    iqsah2 = which( set$surfacearea < qsah[1] | set$surfacearea > qsah[2] )
    if (length( iqsah2)>0 )  set$surfacearea[iqsah2] = NA
    qsa2 = quantile( set$surfacearea, qreject, na.rm=T ) # ~ 0.0028 0.0057
    iqsa2 = which( set$surfacearea < qsa2[1] | set$surfacearea > qsa2[2] ) 
    if (length( iqsa2)>0 )  set$surfacearea[iqsa2] = NA

    iii = which( is.finite(set$surfacearea) & !is.finite( set$sa) )  # surafacearea is determined by automatic mechanisms 
    set$sa[iii] = set$surfacearea[iii]   # sa was determined by Moncton by hand 


    debug = F
    if (debug) {
      # data that need to be hand checked
      iii = which( !is.finite(set$sa) )
      print( "No SA data:" )
      print( set[iii,] )
    }

    sa.thresholds = quantile( set$sa, qreject, na.rm=T )
    distance.thresholds = quantile( set$distance, probs=qreject, na.rm=T )  # recalc after removing unrealistic extremes
    spread.thresholds = c(0.004, 0.016) # initially fixed at realisitc values based upon net configuration 
    
    isp = which( set$spread < spread.thresholds[1] | set$spread > spread.thresholds[2] )
    if (length( isp >0 )) set$spread [ isp] = NA
    spread.thresholds = quantile( set$spread, probs=qreject, na.rm=T ) # recalc after removing unrealistic extremes 
    
    time.thresholds = c(4,10) # hard limits
    ith = which( set$dt < time.thresholds[1]  | set$dt > time.thresholds[2] )  
    if (length( ith >0 )) set$dt [ ith ] = NA
    time.thresholds = quantile( set$dt, probs=qreject, na.rm=T ) # recalc after removing unrealistic extremes

    # check tow distance set extremes to bounds
    il = which(set$distance < distance.thresholds[1] ) 
    if (length(il)>0) {
      set$distance[ il ] = distance.thresholds[1]
      set$sa[il] = set$distance[il] * set$spread[il]
    }
    il = which(set$distance > distance.thresholds[2] ) 
    if (length(il)>0) {
      set$distance[ il ] = distance.thresholds[2]
      set$sa[il] = set$distance[il] * set$spread[il]
    }
   
    # check net spread 
    im = which( set$spread < spread.thresholds[1] ) 
    if (length(im)>0) {
      set$spread[ im ] = spread.thresholds[1] 
      set$sa[ im ] = set$distance[ im ] * set$spread[ im ] 
    }
    im = which( set$spread > spread.thresholds[2] ) 
    if (length(im)>0) {
      set$spread[ im ] = spread.thresholds[2] 
      set$sa[ im ] = set$distance[ im ] * set$spread[ im ] 
    }
  
    for (y in years) {
   
      iy = which( set$yr == y)
      sa.y = mean( boot(set$sa[ iy ], function(x,i) median( x[i], na.rm=T), R=1000 )$t )

      # check tow time /SA 
      io = which( set$yr == y & ( set$dt < time.thresholds[1] |  set$dt > time.thresholds[2] 
              | set$sa < sa.thresholds[1] | set$sa > sa.thresholds[2]  ) )
      
      if (length(io) > 0 ) set$sa[ io ] = sa.y
    }

    ii = which( !is.finite( set$sa ) )
    if (length(ii) > 0 ) set$sa[ ii ] = sa.all

    return (set)
  }



