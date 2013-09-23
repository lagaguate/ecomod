  
  ssa.direct = function( State, Params, Process ) {
    r = with( as.list( c( State, Params ) ), {
      re = rep( NA, length(Process) )
      for ( i in 1:length(re) ) re[i] = eval( Process[i] )
      re
    })
    rtot = sum(r)
    propensity = r / rtot
    j = sample.int( n=length(Process), size=1, replace=FALSE, prob=propensity )
    dtime = - (1/rtot) * log( runif(1) )
    return( list( j=j, dtime=dtime ) )
  }
 

