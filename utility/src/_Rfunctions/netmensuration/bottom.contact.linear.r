
bottom.contact.linear = function( sm, left, right, tdif.min, tdif.max )  {
  ## Smooth method: smooth data to remove local trends and compute first derivatives 

  res = c(NA, NA)

  # use only the subset with data for this step
  names( sm) = c("Z", "timestamp", "ts" ) # Z is used to abstract the variable name such that any variable can be passed

  # find the row numbers which corresponds to each part of the curve, decent, bottom and ascent
  N = nrow(sm)
  buf = 2
  bot = (left+buf):(right-buf)  # blended estimate of fishing events
  down = 1:(left-buf) 
  up =  (right+buf):N

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
   
      #abline (reg=botlm2, col="gray")      
           #abline (reg=uplm2, col="gray")      
           #abline (reg=downlm2, col="gray") 
  duration = as.numeric( difftime(  sm$timestamp[ri2], sm$timestamp[ri1] , units="mins" ) )
  if ( duration > tdif.min & duration < tdif.max ) res =  c( sm$timestamp[ri1], sm$timestamp[ri2] )

  return(res)
}


