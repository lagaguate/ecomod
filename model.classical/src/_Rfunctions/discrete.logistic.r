discrete.logistic = function( ti, S, P ) {
    X = S[1]
    with( P, {
      dX = r * X * (1 - X / K)
      list(c(dX))
    })
  }

