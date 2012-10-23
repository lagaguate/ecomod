
feeding.relations.initialise = function(P) {

 

# ---- after continue with potential ingestion calculations in the simulation part. ... (step 3 in notebook)



    # bacterial activity : small sizes only ...
    P$mineralisationRate = 0.05 / P$timestep  # donor biomass-specific mineralisation ... not used yet
    
    # ingestion rate relative error (in proportion 0,1) 
    P$ingestion.error = 0.5              

    # size-specific photosynrthetic rate ... 
    P$photosynthetic.efficiency = 0.1         # need an estimate here

    # size-specific consumption rate
    P$consumption.efficiency = 0.5            # need a better estimate ... assimilated by predation/grazing , remiander lost as detritus
    

    P$detritusRate = 0.2 / P$timestep         # donor biomass-specific loss to detritus (faeces)
    P$grazing.rate = 0.2 / P$timestep         # donor-specific loss of gross production to heterotrophs 

    # boundary to autotroph flows
    # force boundary inputs to autotrophs to match their outputs (respiration) + grazing loss
    # and so assume ~ steady state
    P$boundaryInflows = rep(0,P$sizeN)
    P$boundaryInflows[ P$autotrophs.i ] = P$sR[ P$autotrophs.i ] + P$grazing.rate

    # losses to boundaries ... respiration and export flux .. 
    # ss assumption ... how about non-ss, migration, advection, etc ..eMatrix
    # alternately sum the product of inputs and a random variable ...
    P$boundaryOutflows = P$sR    # g wet / P$timestep  (heat)
    P$boundaryOutflows[ P$autotrophs.i ] = P$sR[ P$autotrophs.i ] * (1-P$photosynthetic.efficiency) # already in correct time units


    # donor normalised detritivory rates
    P$detritusInflows = rep(0,P$sizeN)
    P$detritusInflows[ P$detritivores.i ] = P$sR[ P$detritivores.i ] # already in correct time units

    # donor biomass-specific flow to detritus (unused mortality + faeces, etc)
    total.detritus = P$detritusRate + P$mortalityRate * (1 - P$consumption.efficiency) 
    P$detritusOutflows = rnorm( n=P$sizeN, mean=total.detritus, sd=total.detritus/4) 

#    P$detritusToBoundary = 0  / P$timestep # no detritus export / mineralisation
#    P$boundaryToDetritus = 0  / P$timestep # no detritus import  



  return (tMatrix)
}
 

