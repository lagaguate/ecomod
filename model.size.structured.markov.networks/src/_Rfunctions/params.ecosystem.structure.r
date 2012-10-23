
params.ecosystem.structure = function(P, biomass0.mean=1, biomass0.sd=0.1, nDonorsMean=10, fractionCarnivorous=0.25, fractionHerbivorous=0.5, fractionDetritivorus=0.25, ingestion.error=0.1, refugia=0.1 ) {
  
  # debug ## 
  # biomass0.mean=1; biomass0.sd=0.1; nDonorsMean=10; fractionCarnivorous=0.25; fractionHerbivorous=0.5; fractionDetritivorus=0.25; ingestion.error=0.1


    # initial biomass configuration
    P$biomass0.mean = biomass0.mean
    P$biomass0.sd = biomass0.sd

    # groups binomial distribution is assumed
    P$fractionHerbivorous = fractionHerbivorous
    P$fractionCarnivorous = fractionCarnivorous
    P$fractionDetritivorus = fractionDetritivorus  # fraction of heterotrophs that are detritivorus
   
    iii = rep(0, P$sizeN) # intialise
    iii[ (P$sizeN-1):P$sizeN ] = NA  
    P$carnivores = P$detritivores = iii
    P$herbivores = rbinom( n=P$sizeN, size=1, prob=P$fractionHerbivorous )
    iii = iii + P$herbivores  # iii=1 are those that have been assigned a category

    # recalc pr. membership given remaining possibilities
    pr.carniv.fixed = P$fractionCarnivorous / ( P$fractionCarnivorous + P$fractionDetritivorus)
    carni.tmp = rbinom( n=length(which(iii==0)), size=1, prob=pr.carniv.fixed  )
    P$carnivores[which(iii==0)] [which(carni.tmp==1) ] = 1 
    iii = iii + P$carnivores

    # remaining must be detritivores ...
    P$detritivores[ which(iii==0)] = 1 

    # flow matrix charateristics (feeding relations)
    P$nDonorsMean = nDonorsMean # the poisson average number of donors to each heterotroph
    P$nConsumersPerDonor = rpois(lambda=P$nDonorsMean, n=P$sizeN)
    P$nConsumersPerDonor[ P$sizeN + 1 ] = sum( P$herbivores, na.rm=T )
    P$nConsumersPerDonor[ P$sizeN + 2 ] = sum( P$detritivores, na.rm=T )

    # create and fill in connecitivity (feeding relations) matrix (binary topology)
    P$ingestion.error = ingestion.error
    P$tm0 = matrix(data=0, nrow=P$sizeN, ncol=P$sizeN)
    
    # heterotrophic donors
    for (donor in 1:(P$sizeN-2)) {
      nConsumers = P$nConsumersPerDonor[donor]
      if (nConsumers > 0) {
        for (i in 1:nConsumers) {
          consumer = floor( runif(n=1,min=1,max=(P$sizeN+1)) )
          if (consumer != donor) cycle
          P$tm0[consumer,donor] = 1
          # cannibalism ?...
    } } }
    P$tm0[ which(P$herbivores==1), (P$sizeN-1) ] = 1 # autotrophs
    P$tm0[ which(P$detritivores==1), (P$sizeN)  ]  = 1 # detritus

    # various mass specific rates :
    # mass balance entails: I = R + P + Eg + Ex
    # and A = R + P
    # and Eg = I - A - Ex = I - R - P - Ex
    P$sR = specific.respiration( P$sizeX )  * P$simulation.stepsize  # "Field" (i.e., real) respiration intensity (mass specific); g wet / timestep
    P$sI = specific.ingestion( P$sizeX ) * P$simulation.stepsize # Ingestion: g wet / year / timestep
    P$sP = specific.growth( P$sizeX ) * P$simulation.stepsize  # Production g wet / year / timestep (somatic growth and reproduction)
    P$sEx = specific.excretion( P$sizeX ) * P$simulation.stepsize  # g wet / year / timestep
    P$sA =  P$sR + P$sP # Assimilation = Standard Respiration rate + Production 
    P$sEg = P$sI - P$sR -P$sP - P$sEx # Egestion = Ingestion - Assimilation - Excretion (i.e., faeces)

    P$refugia = refugia  # biological demand (ingestion) = min(demand, biomass * refugia), then take only refugia

  return (P)

}


