logistic.timevarying.Kr = function( ti, S, P) {  # S=state variable, ti=time, P=parameters
    X = S[1]
    with( P, {
      rt = r * (1 + 0.1*sin(ti*rk  ) )
      Kt = K * (1 + 0.1*sin(ti*Kk  ) )
      dX = rt * X * ( 1 - X / Kt )
      list ( dX )
    })
  }

