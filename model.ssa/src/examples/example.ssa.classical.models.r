
# Simulation using Gillespie algorithm of the master equation for simple birth-death process
# The Gillespie is essentially a Poisson process

# more examples for classical ecological models:
# mostly from: 
# Pineda-Krch M. 2008. GillespieSSA: Implementing the Gillespie 
# Stochastic Simulation Algorithm in R. Journal of Statistical Software 25(12): 1-18. (PDF).


  require(GillespieSSA)
  require (deSolve)



  propensity.logistic = function( cstate, parms, RE ) {
    n.reactions = length(RE)
    reactions = with ( as.list( c(cstate, parms) ), {
      re = rep( 0, n.reactions)
      for ( ir in 1:n.reactions) re[ir] = eval( RE[ir] )
      re
    })
    return( reactions)
  }



# ---------------------------
# Example:  Exponential decay (death process) 
  
  # dX/dt=-cX  { deterministic ODE }                            
  # single reaction channel is:
  # X -> 0 {rd}  ;rd =  reaction rate such that rd * dt = Pr(decay/death)                               
  X0 = 1000
  x0=c(X=X0)
  tf = 100    # time final
  censusInterval = tf/1000
  parms=c(dr=0.5 )    # decay constant
  
  out <- ssa( 
    x0=x0,   # initial states
    a=c("dr*X"),     # propensity (expected number of reactions) given current state
    nu=matrix(-1),  # rows are species (A,B,etc), columns are reaction "channels" (death/decay results in loss of 1 X) 
    parms=parms, # parameter list in same order as a
    tf=tf,           # time of end of simulation in same time units as the reaction rates 
    censusInterval=censusInterval
  )
  ssa.plot(out)


  # ode solution
   # ode solution
  tmax = max( out$data[,1], na.rm=TRUE) 
  times=seq(0,tmax, length.out=100 )
  
  out.ode <- ode(  times=times, y=x0, parms=parms, 
    func= function(Time, State, Pars) {
       with(as.list(c(State, Pars)), {
         dX <- - dr * X
         return(list(c(dX)))
       })
     }
  )
  
  lines( out.ode )



# ---------------------------
# Example: Logistic growth
  # dX/dt = rX(1-X/K)
  # birth = b
  # death = d
  # carrying capacity = K
  # r = b-d 
  parms <- c(b=5, d=1, K=1000)
  x0  <- c(X=500)
  a   <- c("b*X", "(d+(b-d)*X/K)*X")
  nu  <- matrix( c(+1,-1), ncol=2)
  tf = 10
  censusInterval = tf/1000

  out <- ssa( 
    x0=x0, 
    a=a, 
    nu=nu, 
    parms=parms, 
    tf=tf, 
    censusInterval=censusInterval, 
    simName="Logistic growth"
  )
  
  ssa.plot(out)
 

  # ode solution
  tmax = max( out$data[,1], na.rm=TRUE) 
  times=seq(0,tmax, length.out=100 )
  
  out.ode <- ode( times=times , y=x0, parms=parms, 
    func= function(Time, State, Pars) {
       with(as.list(c(State, Pars)), {
         dX <- (b-d)*X *( 1 - X/K) 
         return(list(c(dX)))
       })
     },
     method="lsoda", atol=1e-8
  )
  
  lines( out.ode )
 

  
  # Direct implementation of Gillespie's Algorithm
  loadfunctions( "model.ssa" ) : # load propensity calulator

  n.times = 1000
  t.end = 10
  t.censusinterval = t.end / n.times

  n.processes = 2
  
  Rt = RE = c("b*X", "(d+(b-d)*X/K)*X")
  for (i in 1:n.processes) RE[i] = parse(text=Rt[i])
  parms <- list(b=5, d=1, K=1000)
  nu  <- matrix( c(+1,-1), ncol=n.processes)
  
  out = array( 0, dim=c( n.times ) )
  cstate = c(X=500) 
  simtime = itime = next.output.time = 0

  repeat {
    P = propensity.logistic( cstate, parms, RE )
    P.total = sum(P)
    j = sample.int( n.processes, size=1, replace=FALSE, prob=P/P.total )
    cstate = cstate + nu[j]
    simtime = simtime - (1/P.total) * log( runif(1) )
    if (simtime > t.end ) break()
    if (simtime > next.output.time ) {
      next.output.time = next.output.time + t.censusinterval 
      itime = itime + 1  # time as index
      out[itime] = cstate
    }
  }

  points( seq(0,t.end, length.out=n.times), out, pch=".", col="blue" ) 
  






  # McKane's mean field solution
  #    A -> 0           { death rate = r.death }
  #    A + A -> A       { death/decline rate due to competition = r.competition }      
  #    A  -> A + A      { birth rate = r.birth }  

  # Master equation is:
  # dP(n,t)/dt = T(n|n+1) P(n+1,t) + T(n|n-1) P(n-1,t) - T(n-1|n) P(n,t) - T(n+1|n) P(n,t)

  # mean field (deterministic eq) : by taking the average ==> multiply by n and summing over all n 
  # d<n>_dt = Sum {n=0,N} { T(n+1|n) P(n,t) } - Sum {n=0,N} {T(n-1|n) P(n,t) } 

  # also as,  T(n|n+1) P(n+1,t) ->  T(n-1|n) P(n,t) 
  #   and     T(n|n-1) P(n-1,t) ->  T(n+1|n) P(n,t) 

  # --- not sure how to integrate with the above
  # ... need to think some more about this




# ---------------------------
# Lotka-Volterra predator-prey model
#   Prey::      dN/dt = rN(1-N/K) - aNP/(1+wN)
#   Predator::  dP/dt = P( caN/(1+wN) - g )
  #   prey birth: 0 -> N {bN}                         -- bN     ; per capita birth rate
  #   prey death: N -> 0 {dN}                         -- (d+r*(N/K))*N  ; per capita death 
  #   prey death due to predation:  P + N ->  P  {a}  -- a/(1+w*N)*N*P  ; w= predator saturation (w=0 linear; w>0 saturation)
  #   predator birth:  0 -> P  {bP}                   -- c * a / (1+w*N)*N*P ; c=conversion efficiency
  #   predator death:  P -> 0  {dP}                   -- g * P    ; g per capita death rate of P
  #     (assuming: r = b-d )
  
  parms <- c( b=4, d=1, K=1000, a=0.007, w=0.0035, c=2, g=2)
  x0  <- c(N=1000,P=100)
  a   <- c( 
    "b*N",
    "(d + (b-d)*N/K)*N",
    "a/(1+w*N)*N*P",
    "c*a/(1+w*N)*N*P",
    "g*P"
  )
  nu  <- matrix(c(
    +1, -1, -1,  0,  0,
     0,  0,  0, +1, -1
    ), nrow=2,byrow=TRUE) 
  
  tf = 40
  censusInterval = tf/1000

  out <- ssa( 
    x0=x0,
    a=a,
    nu=nu,
    parms=parms,
    tf=tf,
    censusInterval=censusInterval, 
    simName="Lotka predator-prey model")
  ssa.plot(out)
  
  

  # legend( "topleft", legend= round( mean( out$data[, "N"], trim=.05 ),2) )
 

  # ode solution
  tmax = max( out$data[,1], na.rm=TRUE) 
  times=seq(0,tmax, length.out=1000 )
  
  out.ode <- ode( times=times , y=x0, parms=parms, 
    func=function(Time, State, Pars) {
      with(as.list(c(State, Pars)), {
        saturation = N / (1+w*N)  
        dN = (b-d) * N * (1 - N/K) - a*P*saturation
        dP = c*a*P*saturation - g * P
        return(list(c(dN, dP)))
      })
    }
  )

 
  lines( out.ode[, c(1,2)], col="red", lty="dashed" )
  lines( out.ode[, c(1,3)], col="blue", lty="dashed" )






# ---------------------------
# Example: Kermack-McKendrick SIR model 
     
  parms <- c(beta=0.001, gamma=0.1)
  x0  <- c(S=499,I=1,R=0)
  a   <- c("beta*S*I","gamma*I") 
  nu  <- matrix(c(
    -1, 0,
    +1,-1,
     0,+1
    ),nrow=3,byrow=TRUE)
  
  out <- ssa(x0,a,nu,parms,tf=100,simName="SIR model")
  ssa.plot(out)





# ----------------------------
# Spatial example



U            <- 100
N            <- 500
x0           <- c((N-1), 1, rep(0,(2*U)-2))
names(x0)    <- c(paste(c("S", "I"), floor(seq(1, (U+0.5), 0.5)), sep = ""))

a <- NULL
for (patch in (seq(U))) {
     i <- patch                   # Intra-patch index
     if (patch == 1) j <- U # Inter-patch index
     else j <- patch-1
     a_patch <- c(paste("(1-epsilon)*beta*S", i, "*I", i, "", sep = ""),
                    paste("epsilon*beta*S", i, "*I", j, sep = ""),
                    paste("gamma*I", i, sep = ""),
                    paste("rho*(N-S", i, "-I", i,")", sep = ""))
     a <- c(a, a_patch)
}

###   >>>>>  see sss.spatial.simple.r

