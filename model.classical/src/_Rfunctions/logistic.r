logistic = function( ti, S, P) {  # S=state variable, ti=time, P=parameters
    X = S[1]
    with( P, {
      dX = r * X * ( 1 - X / K )
      list ( dX )
    })
  }

