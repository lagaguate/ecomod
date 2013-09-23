  propensity.spatial.logistic = function( cstate, parms, RE ) {
    
    n.reactions = length(RE)
    
    # rows are easting (x);  columns are northing (y) --- in R 
    nc = ncol(cstate)
    nr = nrow(cstate)

    reactions = with ( as.list( c(cstate, parms)), {
      
      re = NULL
      for ( ir in 1:n.reactions) re = c( re, eval( RE[ir] ) )

      mv.left  = X[,1:(nc-1)] * left 
      mv.right = X[,2:nc] * right   
      mv.down  = X[1:(nr-1),] * down
      mv.up    = X[2:nr,] * up
                      
      re = c(re, mv.left, mv.right, mv.down, mv.up )  # this decomposes by column-first
      re
    })

    return( reactions )
  }

  ## indices: 
  





