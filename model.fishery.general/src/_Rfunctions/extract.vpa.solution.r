extract.vpa.solution = function( oms, vpa.core.model ) {
    
    solution = NULL
    if (!is.null ( oms ) ){
      good = which( oms[,"convergence"] == 0 )
      if (length(good) > 1 ) {
        oms = oms[ good, ]
        final = which.min( oms[,"error"] )
        solution = vpa.core.model( inits= unlist(oms[ final, "par"]) )
      } else if (length(good) == 1) { 
        oms = oms[ good, ]
        solution = vpa.core.model( inits=unlist( oms["par"] ) )
      }
    }
    if (is.null (solution) ) print("Convergence problems")
    return (solution)

}


