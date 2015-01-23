filter.nets = function(DS, x){
  
  if(DS == "doorspread.range")  {
    # doorspread sanity check
    i = which( (x > 90) | (x < 0) ) 
    if (length(i) > 0 ) {
      x[i]=NA
    }
    return(x)
  }
  
  if(DS == "wingspread.range") {  
  # wingspread sanity check
    i = which( (x > 25) | (x < 0) ) 
    if (length(i) > 0 ) {
      x[i]=NA
    }
    return(x)
  }
  
  if(DS == "opening.range") {
    # opening sanity check
    i = which( (x > 10) | (x < 0) ) 
    if (length(i) > 0 ) {
      x[i]=NA
    }
    return(x)
  }

  if(DS == "clearance.range") {
    # clearance sanity check
    i = which( (x > 5) | (x < 0) ) 
    if (length(i) > 0 ) {
      x[i]=NA
    }
    return(x)
  }
  
  if(DS == "depth.range") {  
  # depth sanity check
    i = which( (x > 750) | (x < 0) ) 
    if (length(i) > 0 ) {
      x[i]=NA
    }
    return(x)
  }
  
  if(DS=="remove.trawls.with.US.nets") {
    # removing the trips that fish the US trawl 
    i = grep("NED2014102", x$id)
    x = x[-i,]
    
    mission = "NED2013028"
    setno =c(1, 4, 5, 8, 9, 12, 13, 16, 17, 20, 21, 24, 25, 28, 
             33, 34, 37, 38, 41, 42, 45, 46, 49, 50, 54, 55, 
             58, 59, 60, 64, 65, 68, 69, 72, 73, 76, 77, 80, 81, 
             84, 85, 88, 89, 92, 93, 96, 97, 100, 101, 104, 105, 
             108, 109, 112, 113, 116, 117, 120, 121, 124, 125, 
             128, 129, 132, 133, 136, 137, 140, 141, 144, 145, 
             148, 149, 152, 153,  156,  157,  160,  161,  164,  
             165,  168,  169,  174,  175,  178,  179,  
             182,  183,  186,  187,  190)
    id=paste( mission, setno,sep=".")
    i = which( x$id %in% id)
    x = x[-i,]
    return(x)
  }
  
  if (DS=="door.wing") {
    require(lubridate)
    require(mgcv)
    x$year = year( x$timestamp )
    x$good = TRUE
    
    # first pass -- a linear model to quickly determine large residuals
    dw = lm( wingspread ~ as.factor(year) + doorspread, data=x, na.action="na.exclude" )
    # hist(dw$residuals, "fd")
    x$resids = residuals( dw )
    q.resids = quantile( x$resids, probs=c(0.05, 0.95), na.rm=TRUE )
    x$good[ which( x$resids < q.resids[1] | x$resids > q.resids[2] )] = FALSE
    
    # second pass -- another linear model
    x$wingspread[ !x$good ] = NA
    x$doorspread[ !x$good ] = NA
    
    dw2 = lm( wingspread ~ as.factor(year) + doorspread, data=x, na.action="na.exclude")
    # hist(dw$residuals, "fd")
    x$resids = residuals( dw2 )
    q.resids = quantile( x$resids, probs=c(0.05, 0.95), na.rm=TRUE )
    x$good[ which( x$resids < q.resids[1] | x$resids > q.resids[2] )] = FALSE
    return( x$good )    
  }
  
}