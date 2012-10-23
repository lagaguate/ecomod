
# try also a competitive model of food allocation 

# ----------------------- to do --------------------
# 1. growth model
# 2. recruitment / birth
# 3. move to individual-based ?
# 4. 
# ----------------------- to do --------------------


# Markov-type simulation model of ecological networks
# The size of networks are variable as are the interaction
# strengths and their number. The transition matrix is as follows:

#    columns are donors [row,col]
#    rows are receptors [pred,prey]
 
# Initially, random numbers are generated in terms of the relative portion of biomass utilised by a given species
#
    
  # clear.memory()

	loadfunctions ("model.size.structured.markov.networks")

  basedir = project.directory("model.size.structured.markov.networks") 
  setwd( basedir )

 
  # -----------------------------------------------
  # define parameters and get basic run parameters .. order is somewhat important 
    P = list()
    P = params.time (P, runtype="nonstochastic", simulation.stepsize=0.01, nYears=10 )
    P = params.weights(P, wBase=2, wLower=-12, wUpper=2.5, wStep=0.5)  # define weight bins in grams (wet)
    P = params.size.taxa(P, tDist="poisson", tMean=10, tSD=1, tDecayC=4) # not all needed
    P = params.ecosystem.structure( P, biomass0.mean=1, biomass0.sd=0.1, nDonorsMean=10, fractionCarnivorous=0.6, fractionHerbivorous=0.2, fractionDetritivorus=0.2, ingestion.error=0.1, refugia=0.5 ) # biomass is in grams


 plot( log(P$sizeX), P$sI )
 lines( log(P$sizeX), P$sEg)
 lines( log(P$sizeX), P$sEx)
 lines( log(P$sizeX), P$sR)
 lines( log(P$sizeX), P$sP)
  
 image(P$tm)


#  P = run.params.old( simulation.stepsize=1, nYears=50 ) 
    # simulation.stepsize is in fractions of a year


  # override default params
#  P$runtype = "stochastic"
#  P$runtype = "nonstochastic"
   
  

  # -------------------------
  # Run simulation

    out = run.simulation( P )
    
    res = as.data.frame(out[,1:4])
    colnames(res) = c("time","SP","R","B")
    res$R = res$R / P$simulation.stepsize # express per year 
    res$r_b = res$R/res$B / P$simulation.stepsize   # express per year 

    nss = out[,5:(5+P$wN-1)]
    nss = log(nss, base=P$wBase)
    nss[ !is.finite(nss)  ] = NA
    rm (out)

  # -------------------------
  # Diagnostics/plots

    print( res[c(1,nrow(res)),] )
   
    summary( lm( nss[1,] ~ P$w, na.action="na.omit" ) )
    summary( lm( nss[nrow(res),] ~ P$w, na.action="na.omit" ) )


    plot( P$w, nss[1,], type="l", col="red")
    lines( P$w, nss[nrow(res),], col="blue")
    
    plot( res$B, res$R, pch="." )
    plot( res$R, res$SP, pch="." )
    plot( res$B, res$SP, pch="." )
    plot( res$r_b, res$SP, pch="." )

    plot( res$time, res$R) 
    plot( res$time, res$B) 
    plot( res$time, res$SP)
    plot( res$time, res$r_b) 




