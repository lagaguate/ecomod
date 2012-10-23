
specific.respiration = function(weight) {
  # obtain mass-specific "Field" respiration rates to be used for calculation of total R
  # respiration rates (standard metabolic rates) are from Hemmingsen (1960) 
  # and cited in Peters 1981 (basal metabolic rates in Watts/kg; pg. 31)
   
  # to convert 1 W/kg to X g wet/yr
  # assume: 
  #     1 J = 1 kg m^2 s^-2 = 0.239 cal 
  #     1 W = 1 kg m^2 s^-3 = 1 J s^-1
  #     1 W = 1 J s^-1 * 0.239 cal/J * 60 s/min * 60 min/hr * 24 hr/ day * 365 day/yr
  #         = 7537104 cal/yr 
  #         = 7537.104 kcal/yr
  # Further, 
  #     1 kg wet mass = 7 X 10^6 J ( for fat it is ~ 40 x 10^6 J )
  #                   = 7 X 10^6 J * 0.239 cal / J
  #                   = 1673 kcal
  #     1 kg dry mass = 22 X 10^6 J
  #         
  #  therefore, 1 W/kg 
  #     = 7537.104 kcal/yr / kg 
  #     = 7537.104 kcal/yr / kg * (1 kg / 1673 kcal)
  #     = 4.505143 g wet biomass /year

  weight = weight /1000  # division by 1000 converts to kg
  rInt = c(0.018, 0.140) # for unicells and poiklitherms, respectively the separation is slightly disjunct
  rSlope = 0.751 - 1
  rIntercept = ifelse( weight <= 10^(-8.), rInt[1], rInt[2] ) 
  rSpecific = rIntercept * (weight^rSlope)  
  rSpecific = rSpecific * 4.505143  # convert from Watts/kg to g/g wet year-1 
  rSpecific = rSpecific * 2  # approximately 100% increase in BMR due to digestion (Peters 1981: 143) ... "specific dynamic action" and locomotion, etc :: "Field metabolic rate"
  return (rSpecific)
}



