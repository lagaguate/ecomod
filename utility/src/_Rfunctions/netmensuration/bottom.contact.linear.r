
bottom.contact.linear = function( sm, O )  {
  ## Smooth method: smooth data to remove local trends and compute first derivatives 

  res = c(NA, NA)

  if(0) {
    load("~/ecomod/groundfish/data/nets/Scanmar/bottom.contact/results/bc.NED2014102.8.rdata")
    sm =data.frame( Z=bc$Z)
    sm$timestamp=bc$timestamp
    sm$ts=bc$ts
    good = bc$depth.goodvalues 
    sm$Z[ !good] = NA
    O = bc
  }

  # use only the subset with data for this step
  names( sm) = c("Z", "timestamp", "ts" ) # Z is used to abstract the variable name such that any variable can be passed

  ## at least one solution required to continue  (2 means a valid start and end)
  ## ID best model based upon time .. furthest up a tail is best 
  rsmooth = c(NA, NA)
  rincremental = c(NA, NA)
  rmodal  = c(NA, NA) 
  rintersect = c(NA, NA)

  if ( length(O$smooth.method.indices) > 0) rsmooth = range(O$smooth.method.indices)
  if ( length(O$incremental.method.indices) > 0) rincremental = range(O$incremental.method.indices)
  if ( length(O$modal.method.indices) > 0)  rmodal  = range(O$modal.method.indices)
  # if ( length(O$intersect.method.indices) > 0) rintersect = range(O$intersect.method.indices)

  sol = NULL
  sol = rbind( rsmooth, rincremental, rmodal, rintersect )
 
  left =  median( sol[,1], na.rm=TRUE) 
  right = median( sol[,2], na.rm=TRUE)

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

  # find the row numbers which corresponds to each part of the curve, decent, bottom and ascent
  N = nrow(sm)
  buf = 2 ## num of overlapping indices at intersection
  bot = (left-buf):(right+buf)  # blended estimate of fishing events
  down = 1:(left+buf) 
  up =  (right-buf):N
 
  if(any(down < 1))  down = down [ -which(down < 1) ] 
  if(any(up > N))  up = up [ -which( up > N) ] 
  #  compute linear models for each section
  botlm2 = lm( Z ~ ts, sm[bot, ], na.action = "na.exclude")
  #  right tail
  ri2 = N
 
  if (( length( which( is.finite( sm$Z[bot] ))) > 10) & (length( which( is.finite(sm$Z[up]))) > 5 )) {
    uplm2 = lm( Z ~ ts, sm[up, ], na.action = "na.exclude")
    cm <- rbind(coef(botlm2),coef(uplm2)) # Coefficient matrix
    i2=c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
    ii = which(sm$ts >i2[1]) 
    if (length(ii)>0) ri2 = min( ii )
  }

  # left tail
  ri1 =  1
  if (( length( which( is.finite( sm$Z[bot] ))) > 10) & ( length( which( is.finite( sm$Z[down]))) > 5 )) {
    downlm2 =lm( Z ~ ts, sm[down,], na.action = "na.exclude")
    # find where the sections intersect
    cm <- rbind(coef(botlm2),coef(downlm2)) # Coefficient matrix
    i1=c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
    ii = which(sm$ts < i1[1])
    if (length(ii)>0) { 
      ri1 = max(ii)
    } 
  } 
   
  res =  c( sm$timestamp[ri1], sm$timestamp[ri2] )

  return(res)
}


