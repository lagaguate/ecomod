
  metabolic.rates = function( mass.g, temperature.C=NULL ) {
      
    # require mass (g), temperature.C (C)
    # from Robinson et al. (1983) 
    # specific standard MR 
    #   = smr 
    #   = 0.067 M^(-0.24) * exp(0.051 * Temp.Celcius) 
    # (Temp.Celcius; M in grams, MR in ml O2/g/hr)
    #
    # Converting to more standard units:
    # 1 ml O2 = 4.8 cal (from paper)
    # and 1 W = 7537.2 kcal/yr
    # 1 ml O2 / g / hr = 4.8 * 24 * 365 kcal / yr / kg
    #                  = 4.8 * 24 * 365 / (7537.2 W) / kg
    #                  = 5.57873 W / kg
    

    # noting similarity to Arrehnius formulation:
    #   smr ~ A * exp( -Ea / (K * T) )
    #     where:  Ea = energy of activation {	kcal/mol or J/ mol } 
    #             R  = 1.985877534 × 10−3	kcal/mol (Gas constant) or kB if on per molecule basis
    #             kB = 1.3806488(13)×10−23 J/K 
    #             T  = temperature in Kelvin
    # then,         
    #   A  ~ 0.067 * M^(-0.24) / 5.57873  { W / kg }
    #   Ea ~ -b2 * T.C * R (T.C +273.15)  
    
    if (is.null(temperature.C)) temperature.C=10
    tmed = median( temperature.C, na.rm=T )
    oo = which(!is.finite( temperature.C) )
    if (length(oo)>0) temperature.C[oo] = tmed

    N.avogadro = 6.0221412927 * 10^23 ## mol−1
    kB = 1.380648813 * 10^-23 # Boltzman's constant  J/K  
    # K =  kB * N.avogadro # R or kB , here directly R = kB {J/K} * N.avogadro {n/mol}  muiliplied to give units of J / K / mol
    K = 8.314462175 # as above to greater precision
    b0 = 0.067
    b1 = -0.24
    b2 = 0.051
    from.ml.O2.per.g.per.hr.to.W.per.kg = 1 / 5.57873 

    # Arrhenius parameterization:
    # the pre-factor -> total number of collisions ... ie. similar to encounter rate 
    A = b0 * (mass.g)^b1 * from.ml.O2.per.g.per.hr.to.W.per.kg  
    
    # 'activation energy' 
    Ea =  - b2 * temperature.C * K * ( temperature.C + 273.15 ) 
    
    # fraction of collisions leading to a reaction (ie. a metabolic event -- e.g. dissipation) .. reacting or Pr of reaction -- incorporates (i.e., due to) temperature influence 
    Pr.Reaction = exp( -Ea / (K * (temperature.C + 273.15) ) ) ## == exp( b2 * temperature.C ) 

    smr = A * Pr.Reaction  #  == b0 * (mass.g)^b1 * exp(  b2 * temperature.C ) * from.ml.O2.per.g.per.hr.to.W.per.kg
    mr = smr * mass.g

    x = data.frame( smr=smr, mr=mr, Ea=Ea, A=A, Pr.Reaction=Pr.Reaction ) 
    
    ## units:
    ## smr,A ~ {W/kg}; 
    ## mr~{W}; 
    ## Ea ~ {J/mol} energy per per mol

    return(x)
  }
  
  
