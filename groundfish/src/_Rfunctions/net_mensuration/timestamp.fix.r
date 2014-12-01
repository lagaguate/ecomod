timestamp.fix = function(  tstamp, threshold.hrs=2 ) {
  
  # fix sets that cross midnight
  # test to see if the time range is greater than threshold ... in groundfish 30 min tows (test on 2 hr) 
  # identify the indices where times are too large and adjust the day values
  
  tocorrect=NULL
  hr2sec = 60*60 
  threshold.seconds = hr2sec * threshold.hrs

  r = range(tstamp, na.rm=TRUE)
  y = as.duration(new_interval(r[1],r[2]))  # in seconds
    
  if ( y > threshold.seconds ) { # as duration is in seconds
    # if there is a timestamp problem, the problematic records are those with hour values that are soon after midnight 
    # .. assume any values from midnight to 2 AM need to be recoded to the next day's value
    hrs = hour( tstamp )
    z = which( hrs < 2 )  # 2AM is the cutoff 
    if ( length(z) > 0 ) {
      day( tstamp[z]) = day( tstamp[z])+1
    }
  }  
  return( tstamp)
}
