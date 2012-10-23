lotka.volterra.matrix = function( ti, S, P ) {
    with( P, {
      dn = r * S + S * (A %*% S)
      list(c(dn))
    })
  }

