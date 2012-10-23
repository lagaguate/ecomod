
simul.nonstochastic = function(P) {

    i.detritus = P$sizeN  # index of detritus in the J-matrix
    i.autotrophs = P$sizeN - 1 # index of autotrophs in the J-matrix
    i.living = c(1:(P$sizeN-2))
    i.all = c(1:P$sizeN)

    additional.parameters = 4
    ncells = length( P$tm )
    tend = P$nYears / P$simulation.stepsize
    biomass = biomass0 = rnorm( n=P$sizeN, mean=P$biomass0.mean, sd=P$biomass0.sd )  # random normal
    
    present = rep( 1, P$sizeN )
    out = matrix( NA, ncol=additional.parameters+P$wN, nrow=tend )
    
    # J is the ingestion matrix (potential transfers available, ie,  ~ Jacobian)  ... must fill in for detritus and autotrophs
    J = matrix(data=0, nrow=P$sizeN, ncol=P$sizeN) # initialise

    # ---------------
    # affinity ( diet matrix) matrix ... each row represents the proportion of the diet
    A = P$tm0 * rnorm( length(P$tm0), mean=1, sd=P$ingestion.error )         
    for (r in i.all ) A[r,] = A[r,] / rowSums(A, na.rm=T)[r] # normalise affinity to sum of 1 for each row   
      
    for ( ti in 1:tend ) {

      # initial estimates of mass flux demanded by biomass(t)
      respiration = P$sR * biomass
      excretion = P$sEx * biomass
      egestion =  P$sEg * biomass
      # assimilation = P$sA * biomass
      # production = P$sP * biomass
      ingestion0 = P$sI * biomass  # (g/g/time step) * (g) = g / time step; total ingestion required (demanded) by each consumer
   
      # re-scale ingestion, egestion and excretion relative to what is actually there to support the demand 
      J[] = 0 # reinitialise
      for (r in i.living)  J[r,] =  A[r,] * ingestion0[r]  # row-wise multiplication: ingestion for each consumer
      J[  which(P$detritivores==1), i.detritus] =  ingestion0[ which(P$detritivores==1) ]
      J[  which(P$herbivores==1), i.autotrophs] = ingestion0[ which(P$herbivores==1) ]
      mortality = colSums( J[i.living, ] ) # mostly consumptive mortality/losses
      available = (1-P$refugia) * biomass 
      ingestion.correction = available / mortality # calculate normalisation factor
      ingestion.correction[ which(!is.finite( ingestion.correction ) ) ] = 0

      di = which( mortality > available ) # indices of those with mortality greater than available biomass
      if (length(di) > 1) { # reduce mortality to an upper limit 
        for (c in i.all) J[,c] =  J[,c] * ingestion.correction[c] # normalised ingestion for each resource by what is available
        ingestion.refactor = rowSums(J, na.rm=T) / ingestion0
        excretion[di] = excretion[di] * ingestion.refactor[di]
        egestion[di]  = egestion [di] * ingestion.refactor[di]
        # respiration is not corrected as it is assumed that this is a basic biological cost 
        # ... this is not completely true as a some fraction is associated with consumption/assimilation
      }
     
      # boundary inputs 
      # currently, steady state assumption if 
      #     bo. inputs (i.e. plant biomass ingested) = bo outputs  (total heterotrophic comm respiration)
      # alternates: 10 * total heterotrophic production (use P/B ratio?)  # input into autotrophic biomass .. etc
      J[i.autotrophs,] = respiration 

      # detrital flows
      J[i.detritus,] = excretion + egestion  # detritus accumulation
      
      # final fixes assuming no direct interaction between detritus and autotrophs (ie. only through the intermediary action of heterotrophs)
      J[ c(i.autotrophs, i.detritus), c(i.autotrophs, i.detritus) ] = 0




      inputs = rowSums( J, na.rm=T ) # mostly ingestion + inputs to detritus and autotrophic component
      mortality = colSums( J[ i.living, ], na.rm=T ) # recalculate mortality  .. last two rows (boundary inputs) have yet to be calcuclated ... next
      outputs = colSums(rbind(mortality, respiration, excretion, egestion), na.rm=T)
      biomass = biomass + inputs - outputs 

      numbers = biomass / P$sizeX
      below.threshold = which( (numbers < P$lowerLimit)  & is.finite(numbers) )
      biomass[ below.threshold ] = 0
      numbers[ below.threshold ] = 0 
      present[ below.threshold ] = NA

      richness = sum(present, na.rm=T)

      B = sum( biomass[ i.living ], na.rm=T )   # biomass of the heterotrophs only
      R = sum( respiration[ i.living], na.rm=T ) # biomass of the heterotrophs only

      wN = rep( 0, P$wN )
      for (i in 1:P$wN) wN[i] = sum( numbers[ which( P$w[i]==P$size )] )
      
      out[ ti, ] = c( round(ti*P$simulation.stepsize,3), richness, R, B, wN )

    }

  return (out)

}


