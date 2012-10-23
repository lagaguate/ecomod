lotka.volterra.2D.diffusion = function ( ti, S, P, N, Da, dx ) {
    NN = N*N
    Prey = matrix(nr = N, nc = N, S[1:NN])
    Pred = matrix(nr = N, nc = N, S[(NN+1):(2*NN)])

    with ( P, {
      ## Biology
      dPrey   = rGrow* Prey *(1- Prey/K) - rIng* Prey *Pred
      dPred   = rIng* Prey *Pred*assEff -rMort* Pred

      zero = rep(0,N)

      ## 1. Fluxes in x-direction; zero fluxes near boundaries
      FluxPrey = -Da * rbind(zero,(Prey[2:N,]-Prey[1:(N-1),]), zero)/dx
      FluxPred = -Da * rbind(zero,(Pred[2:N,]-Pred[1:(N-1),]), zero)/dx

      ## Add flux gradient to rate of change
      dPrey    = dPrey - (FluxPrey[2:(N+1),]-FluxPrey[1:N,])/dx
      dPred    = dPred - (FluxPred[2:(N+1),]-FluxPred[1:N,])/dx

      ## 2. Fluxes in y-direction; zero fluxes near boundaries
      FluxPrey = -Da * cbind(zero,(Prey[,2:N]-Prey[,1:(N-1)]), zero)/dx
      FluxPred = -Da * cbind(zero,(Pred[,2:N]-Pred[,1:(N-1)]), zero)/dx

      ## Add flux gradient to rate of change
      dPrey    = dPrey - (FluxPrey[,2:(N+1)]-FluxPrey[,1:N])/dx
      dPred    = dPred - (FluxPred[,2:(N+1)]-FluxPred[,1:N])/dx

      list(c(as.vector(dPrey), as.vector(dPred)))
   })
  }

