
bottom.contact.linear = function( sm, O, bcp )  {
  ## Smooth method: smooth data to remove local trends and compute first derivatives 

  res = c(NA, NA)

  if(0) {
    load("~/ecomod/groundfish/data/nets/Scanmar/bottom.contact/results/bc.NED2014102.8.rdata")
    sm =data.frame( Z=bc$Z)
    sm$timestamp=bc$timestamp
    sm$ts=bc$ts
    good = bc$good 
    sm$Z[ !good] = NA
    O = bc
  }

  # use only the subset with data for this step
  names( sm) = c("Z", "timestamp", "ts" ) # Z is used to abstract the variable name such that any variable can be passed

  # minor cleaning and linear interpolation of missing data
  sm$Z = interpolate.xy.robust( sm[, c("ts", "Z")],probs=bcp$linear.filter.quants, trim=bcp$linear.trim,  method="sequential.linear" )
 
  ## at least one solution required to continue  (2 means a valid start and end)
  ## ID best model based upon time .. furthest up a tail is best 

  sol = NULL
  if ( length(O$smooth.method.indices) > 0) sol = rbind( sol, range(O$smooth.method.indices) )
  if ( length(O$modal.method.indices) > 0)  sol = rbind( sol, range(O$modal.method.indices) )
  if ( length(O$maxdepth.method.indices) > 0)  sol = rbind( sol, range(O$maxdepth.method.indices) )
    
  left =  mean( sol[,1], trim=0.1, na.rm=TRUE) 
  right = mean( sol[,2], trim=0.1, na.rm=TRUE)

  ### NOTE:: these indices are relative to the original data sent to bottom.contact 
  ###        and not "sm" which is what is sent into this function
  ###        So ... recover the correct indices by adding the offset associated with sm
  left = left - min(O$aoi) + 1
  right = right - min(O$aoi) + 1
  
  if (0) {
    plot( Z~ts, sm, pch="." )
    abline( v= sm$ts[ c( left, right)], col="red" )  # initial breaks
  } 

  if ( any( !is.finite( c(left, right) ) ) ) return(res)
  
  # remember sm is is the bounds delimited by variance indices so 1=start of variance on left, N is end of variance right
  N = nrow(sm)  
  down = 1:left 
  up =  right:N
  dl = left
  dr = N-right
  # line segments are approximately same length as the up,down segments
  bot_left = c(left:(left+2*dl))
  bot_right = c( (right-2*dr):right)

  # find the row numbers which corresponds to each part of the curve, decent, bottom and ascent
  if(any(down < 1))  down = down [ -which(down < 1) ] 
  if(any(up > N))  up = up [ -which( up > N) ] 
  #  compute linear models for each section and their intersection

  #  right tail
  ri2 = N
  if (( length( which( is.finite( sm$Z[bot_right] ))) > 10) & (length( which( is.finite(sm$Z[up]))) > 5 )) {
    uplm2 = lm( Z ~ ts, sm[up, ], na.action = "na.exclude")
    botlm2r = lm( Z ~ ts, sm[bot_right, ], na.action = "na.exclude")
    cm <- rbind(coef(botlm2r),coef(uplm2)) # Coefficient matrix
    i2=c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
    ii = which(sm$ts >i2[1]) 
    if (length(ii)>0) ri2 = min( ii )
  }

  # left tail
  ri1 =  1
  if (( length( which( is.finite( sm$Z[bot_left] ))) > 10) & ( length( which( is.finite( sm$Z[down]))) > 5 )) {
    downlm2 =lm( Z ~ ts, sm[down,], na.action = "na.exclude")
    botlm2l = lm( Z ~ ts, sm[bot_left, ], na.action = "na.exclude")
    # find where the sections intersect
    cm <- rbind(coef(botlm2l),coef(downlm2)) # Coefficient matrix
    i1=c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
    ii = which(sm$ts < i1[1])
    if (length(ii)>0) { 
      ri1 = max(ii)
    } 
  } 
   
  res =  c( sm$timestamp[ri1], sm$timestamp[ri2] )

  return(res)
}

