  
	# based upon the FAO tutorials, the following are the worked examples

  loadfunctions( "model.fishery.general" )
  
  V = get.vpa.data() # test data from cod assessments

# this returns: 
#   S = stock (ageXyear)
#   F = fishing mortality (ageXyear)
#   B = biomass (ageXyear)
#   SSB = spawing stock biomass (ageXyear)
#   catch = total landings (ageXyear)
#   ages = ages of fish
#   nages = number of ages
#   p = index of plus group
#   aX = indice of the last two age groups (7,8)
#   a = indices of all age groups except aX
#   years = years of catch data
#   nyears = number of years
#   years.rv = years of the RV data
#   nyears.rv = number of year of the RV data
#   yX = index of the last year
#   y = index of all years except yX
#   weight.kg = weight of each age class by catch and stock a
#   M = natural mortality rate
#   MOG = maturity ogive
# -------------------------------
 
   if (Pope.cohort.analysis) {
    # F at age is assumed constant for 7 and 8+, with the 
    # remainder of the cohorts to be calculated using Pope's population model.
   
    # unpack parameters
    ages = V$ages
    years = V$years
    nages = length(ages)
    nyears = length(years)
    aX = c( nages-1, nages) # indices of the last 2 age classes (8 plus group and age 7)
    yX = nyears    # the index of the last year
    a = c(1:(nages-2))
    y = c(1:(nyears-1))
    
    V$F = V$catch * NA # intialise
    V$F[ aX , ] <- 0.5 
    V$F[ , yX ] = c(0.1, 0.2, 0.4, 0.5, 0.5, 0.5, 0.5 )
    V$S = cohort.analysis( V$catch, V$F, V$M  )
    V$F[ a,y ] = log( V$S[ a, y] / V$S[ a+1, y+1]) - V$M[ a ] # update F estimates after convergence
    V = calculate.biomass( V )
    plot.additional.vpa.results( V )
  }

 # -------------------------------

# base template of an ADAPT-type of population assessment
# inital conditions as Pope's cohort model but:
# optimise residual SS of real catch and theoretical catch
# by decomposing F into an age-specific and a year-specific component
# inital conditions of F for optimisation
  F.age.init = c(0.1, 0.2, 0.3, 0.4, 0.7, 1, 1)  # increasing fishing mortality with age init
  F.year.init = rep( 0.5, length(V$years))  # 0.5 equal is fishing mortality init
  inits = c( F.age.init, F.year.init ) 
  lower = rep(0.001, length(inits) )  
  upper = rep(10, length(inits) )  
  
# optim.methods = c( "Nelder-Mead", "CG", "BFGS", "L-BFGS-B", "SANN", "PORT", "NLM-Newton" )
  optim.methods = c( "Nelder-Mead", "SANN", "PORT", "NLM-Newton" ) # PORT seems most robust
  
  vpa.params = find.optim.solution (optim.methods=optim.methods, vpa.model=adapt.optim )  
      # other examples include: ICA.optim, sep.VPA.optim
  V = extract.vpa.solution (vpa.params, vpa.core.model=adapt.core)
      # vpa.core.model must match the wrapper function vpa.model ...eg, sep.VPA.core, ICA.core

  V = calculate.biomass( V )  # complete some estimates
  plot.additional.vpa.results( V ) # a few plots


