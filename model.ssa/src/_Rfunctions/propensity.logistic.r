 
  propensity.logistic = function( cstate, parms, RE ) {
    n.reactions = length(RE)
    reactions = with ( as.list( c(cstate, parms) ), {
      re = rep( 0, n.reactions)
      for ( ir in 1:n.reactions) re[ir] = eval( RE[ir] )
      re
    })
    return( reactions)
  }


