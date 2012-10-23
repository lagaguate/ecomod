
 
  biomass.logistic.schnute.noequilibrium = function( O, C ) {
    # surplus production model estimation using Schnute's Nonequilibrium method  
    # begin with Fletcher's parameterization of the logistic equation:
    #   dB/dt = 4m/K(1-B/K)
    # then integrate dO/dt/O ; O = qB
    # ln( O[t+1]/O[t] ) = 4 * m / K - 4 * m / (q *K^2) *(O[t] +O[[t+1])/2 - q*(E[t]) 
    # and solved via regression y = a + bx1 + cx2
    #  as E = effort = C/O
    # O is the observed index of abundance (e.g. biomass or CPUE)
    # C is the observed catch (yield)
    # B is the "true" / modelled biomass = q*O 
    # P = annual surplus production
    # see Quinn and Deriso 1999: p 68 

    nx = length(O)
    ii = 2:nx
    jj = 1:(nx-1)
    y  = log( O[ii] / O[jj] ) 
    x1 = (O[ii] + O[jj]) /2 
    x2 = (C[ii]/O[ii] + C[jj]/O[jj]) /2

    mod = lm( y ~ x1 + x2 )
    co = coef(mod)
    q = co["x2"]
    K = -co["(Intercept)"]/(co["x1"] *co["x2"])
    m =  co["(Intercept)"]^2/(4 * co["x1"] *co["x2"])
    r = 4 * m / K
        
    out = data.frame( cbind( r=r, K=K, q=q, MSY=m ), row.names="Schnute's nonequilibrium method" )

    return( out )
  }


