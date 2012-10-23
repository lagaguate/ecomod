
simul.stochastic = function(P) {
  
    additional.vars = 6
    ncells = length(P$tm)
    tend = P$timestep*P$nYears
    biomass.initial = 1
    biomass = biomass0 = rnorm(n=P$sizeN, mean=P$biomass0.mean, sd=P$biomass0.sd )  # random normal 
    present = rep(1, P$sizeN)
  #  .Det = .Bound = .In = .Out = rep( 0, P$sizeN )
    out = matrix(NA, ncol=additional.vars+P$wN, nrow=tend)
    
    for ( ti in 1:tend ) {

      eM = matrix( rnorm(n=ncells, mean=0, sd=P$ingestion.error ), nrow=P$sizeN, ncol=P$sizeN ) * P$simulation.stepsize # Z-score distribution
      tC = P$tm + eM 
      tC[ which( tC < 0 )] = 0
      detritus.error = rnorm(n=P$sizeN, mean=0, sd=P$biomass0.sd) * P$simulation.stepsize
      boundary.error = rnorm(n=P$sizeN, mean=0, sd=P$biomass0.sd) * P$simulation.stepsize



    ###### the remainder is essentially the same as the non-stochastic form
  
      inputs  = as.vector(tC %*% biomass)
      outputs = as.vector(biomass %*% tC)
      detritus = biomass *  (P$detritusInflows - P$detritusOutflows) + detritus.error
      boundary = biomass *  (P$boundaryInflows - P$boundaryOutflows) + boundary.error

      biomass =  biomass + inputs - outputs + detritus + boundary
      numbers = biomass / P$sizeX
      
      below.threshold = which( numbers < P$lowerLimit)
      
      biomass[ below.threshold ] = 0
      numbers[ below.threshold ] = 0 
      present[ below.threshold ] = NA

      richness = sum(present, na.rm=T)

      B = sum(biomass)
      R = sum(P$sR * biomass)
      dets = biomass[ which( ) ]

      wN = rep( 0, P$wN )
      for (i in 1:P$wN) wN[i] = sum( numbers[ which( P$w[i]==P$size )] )
      
      out[ ti, ] = c( round(ti * P$simulation.stepsize,3), richness, R, B, wN )

  #    .In = .In + inputs
  #    .Out = .Out + outputs
  #    .Det = .Det + detritus
  #    .Bound = .Bound + boundary

    }

  return (out)

}


