

  biomass.logistic.recursion = function( par, O, C, errorType="lognormal" ) {
    # surplus production model estimation using maximum likelihood/optimization methods
    # observation error only recursive difference equation method
    
    # AKA, "biomass dynamics model" or "Schaeffer" model 
      
    # based upon equations in Hilborn & Wlaters (1992) - Chapter 8
    # biomass dynamics model for snow crab (AKA, surplus production, production models)
    # simple Schaefer (1954) formulation / extensions by Pella-Tomlinson (1969):
    # basic equation (discrete): 
    # 
    # B(t+1) = B(t) + R(t) + G(t) - C(t) - M(t) 
    # where: t is time, B is biomass, R is recruitment, G is growth, C is catch and M is natural mortality
    # let: P = G + R; where P is production
    # B(t+1) = B(t) + P(t) - M(t) ; when there is no catch (C)
    # let: S  = P - M
    # where S = "surplus production", the production (biomass increase) in the absence of fishing 
    # that is, the amount that can be caught while maintaining constant B (at equilibrium):
    # B(t+1) = B(t) + S(t) - C(t) 
    # or,
    # S = B(t+1) - B(t) + C(t)
    # Schaefer's formulation:
    # dB/dt = rB(1- B/K) - C ; r is intrinsic rate of increase of B, K=unfished equilibrium stock size
    # assuming: 
    # C(t) = q * E(t) * B(t) ; where q = "catchability", E is fishing effort 
    # thus, O ~ U = C/E = qB  ; where O = observed index of abundance 
    # U is catch per unit effort or any observed index of abundance 
    # or B = C/E/q 
    # ( Aside: Pella-Thomlinson adds an exponent (m) to the biomass term (B -> B^m) 
    # such that if m=2 it becomes the Sahfer forumaltion)
    # Estimation possible via:
    # (1) equilibrium methods (unstable); 
    # (2) Regression (Schnute 1977; erratic); 
    # (3) Observation error or "time-series method" (recommended by Hilborn & Walters )
    # using method (3a): using cpue or biomass index 
    # B(t) = B(t-1) + r * B(t-1) * ( 1 - B(t-1) / K ) - C(t-1)
    #      = B(t-1) * ( 1 + r * ( 1- B(t-1)/K ) - C(t-1) 
    # O(t) = q * B(t)
    # e(t) = (O(t) - <O(t)> )^2 ; where < > is the estimated value of CPUE or other abundance metric (eg survey) ::: "observation error"
    # minimize( sum(e(t)) ) 
    # or,
    # method (3b): using effort only
    # C(t) = B(t) * q * E(t)
    # e(t) = (C(t) - <C(t)> ) ^2 ;  where < > is again the predicted/estimated value
    # B(0) must be estimated or one can assume:
    # B(1) = C(t) / ( E(1) * q ) ; or 
    # B(1) = a running average of the start of the C(1..5, etc); or 
    # B(0) == K (if starting with an unfished population .. 

    # B is normalised by K to improve convergence
    # O is the observed index of abundance (e.g. biomass or CPUE)
    # C is the observed catch
    # B is the "true" / modelled biomass = q*O

    eps = 1e-4 
    
    r=max( eps, par["r"] ) # intrinsic rate of increase of biomass
    K=max( eps, par["K"] ) # carrying capacity
    q=max( eps, par["q"] )# catchability  -- force in interval (0,1) 
    B0=par["B0"] / K # starting model abundance
    
    nx = length(O) # no. data points in Time Series
    B = rep( 0, nx ) # model biomass
    B[1] =  B0 * (1 + r * ( 1 - B0 )) - C[1]/K  # initial conditions 
    for ( t in 2:nx)  {
      B[t] = max( eps, B[t-1] * ( 1 + r * ( 1 - B[t-1] )) - C[t-1]/K )
    }
    
    O = O/K
    Op = q * B  # modelled index of abundance
  
    # normal and lognormal errors are coded .. for others, the underlying function needs a minor modification:

    if (errorType == "normal" ) error = sum ( (O - Op)^2, na.rm=T )
    if (errorType == "lognormal" ) error = sum ( (log(O) - log(Op))^2, na.rm=T )

    return( error )
  }



