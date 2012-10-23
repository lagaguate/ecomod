lotka.volterra.2 = function ( ti, S, P ) {
    # S[1] = prey
    # S[2] = predator 
    with( P, {
      dX = r1 * S[1] * (1-S[1]/K1) - m1 * S[1] * S[2]   # growth - mortality
      dY = ae2 * m1 * S[1] * S[2]  - m2 * S[2] # growth - mortality
      list( c(dX, dY) )
    })
  }
    # encouter.rate = 0.2/day, rate of ingestion
    # growth rate = 1.0/day, growth rate of prey
    # predator mortality = 0.2/day, mortality rate of predator
    # assimilation efficiency = 0.5,    # -, assimilation efficiency
    # carrying capacity = 10     # mmol/m3, carrying capacity

