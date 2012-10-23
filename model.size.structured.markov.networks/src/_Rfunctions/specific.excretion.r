
specific.excretion = function(weight) {   # ---------- NOTE ... does not make sense .. the rates are very low .. check this
  # obtain specific excretion rates for ecottherms
  # from Peters et al 1996 (in Watts/kg, Table 5 )
  # in the paper: units of excretion (Ex) is J/day and weight (w) is in grams fresh weight
  # for ectotherms: Ex = 0.360 * w ^(0.701); R^2 = 0.53, n=46
   
  weight = weight /1000  # division by 1000 converts to kg

  Intercept = 0.36 
  Slope = 0.75  - 1
  specific.excretion = Intercept * (weight^Slope) # division by 1000 converts weight from g to kg
  specific.excretion = specific.excretion /86400 * 4.505143  # convert from J/s to Watts to g wet year-1 
  return (specific.excretion)
}


