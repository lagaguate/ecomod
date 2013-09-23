
  rate.process = function( n, rate ) {
    ## simulate a rate to a discrete event 
    ## n = number of replicates ...  
    ## using rbinom is 1/6 slower 
    
    events = rbinom(n, 1, rate )  
    return(events)
  }


