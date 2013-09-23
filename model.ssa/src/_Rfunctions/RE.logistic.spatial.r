

  RE.logistic.spatial = function( proc ) {
    # Reaction processes ...
    switch( proc, 
      "b*X[jr,jc]" ,  # birth
      "(d+(b-d)*X[jr,jc]/K)*X[jr,jc]" ,   #  death
      "dr*X[jr,jc]" ,   # in balance equation: X[i] <-> X[i+1] :: therefore, this is X[i] -> X[i+1] {dr0}
      "dr*X[jr,jc]" ,   # and this is the coupled:  X[i+1] -> X[i] {dr1}
      "dc*X[jr,jc]" ,   #diffusion
      "dc*X[jr,jc]"  
    )

  }


