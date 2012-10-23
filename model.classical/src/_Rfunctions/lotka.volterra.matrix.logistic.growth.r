lotka.volterra.matrix.logistic.growth = function( ti, S, P ) {
    with( P, {
      dn = r * S* ( 1 - A %*% S)
      list(c(dn))
    })
  }

