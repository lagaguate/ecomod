  # Changes associated with Reaction processes 
  # Lagrangian operator structure: (row, column, operation) where 0,0 is the focal cell and the last element is the operation
  NU.logistic.spatial = list ( 
      rbind( c(0,0,1) ),  # for the focal cell (0,0), the birth process: "bX"
      rbind( c(0,0,-1) ), # for the focal cell (0,0), the death process: "(d+(b-d)*X/K)*X""
      rbind( c(0,0,-1), c(-1,0,1) ), # "jump to adjacent row from the focal cell:: X[i] -> X[i+/-1] {dr0}
      rbind( c(0,0,-1), c(+1,0,1) ),
      rbind( c(0,0,-1), c(0,-1,1) ),  # same as above but now for column-wise jumps
      rbind( c(0,0,-1), c(0,+1,1) )
  )


