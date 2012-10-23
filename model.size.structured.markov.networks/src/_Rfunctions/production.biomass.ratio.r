
production.biomass.ratio = function(weight) {  # ~ r, intrinsic rate of increase
  # obtain specific P:B ratios from Fenchel (1974), quoted in Peters 1981 (in Watts/kg; pg 134 + appendix 8c, pg 284)
  weight = weight /1000  # division by 1000 converts to kg

  # params: unicells, poikilotherms
  Intercept = c(0.136, 0.281) 
  Slope = c(-0.28, -0.27 )
  pbIntercept = ifelse( weight <= 10^(-8.), Intercept[1], Intercept[2] )
  pbSlope = ifelse( weight <= 10^(-8.), Slope[1], Slope[2] )

  prod.biomass = pbIntercept * (weight^pbSlope)
  prod.biomass = prod.biomass * 4.505143  # convert from Watts/kg to year-1 
  return (prod.biomass)
}
 

