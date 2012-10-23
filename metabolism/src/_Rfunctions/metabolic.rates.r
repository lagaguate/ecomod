
  metabolic.rates = function( mass.g, temperature.C=10, tK=10 ) {
      
    # require mass (g), temperature.C (C)
    # from Robinson et al. (1983) 
    # specific standard MR = 0.067 M^(-0.24) * exp(0.051 * Temp) 
    # (Temp in deg Celcius; M in grams, MR in ml O2/g/hr)
    # 1 ml O2 = 4.8 cal (from paper)
    # and 1 W = 7537.2 kcal/yr
    # 1 ml O2 / g / hr = 4.8 * 24 * 365 kcal / yr / kg
    #                  = 4.8 * 24 * 365 / (7537.2 W) / kg
    #                  = 5.57873 W / kg
    # assume a constant temperature tK (10 C) if none given

    b0 = 0.067
    b1 = -0.24
    b2 = 0.051
    from.ml.O2.per.g.per.hr.to.W.per.kg = 1 / 5.57873
    

    smr = b0 * (mass.g*1000)^b1 * exp( b2 * tK ) * from.ml.O2.per.g.per.hr.to.W.per.kg
    mr = smr * mass.g

    # Arrhenius correction
    k = 10^8 # the constant
    Ea = 10 # energy of activation (kcal/mol) 10 ~ q10 of 2; 25 ~ q10 of 4
    R = 1.9858775 # gas constant cal/(mole.K)
    ArrheniusFactor = k * exp( - Ea * 1000 / (R * ( (temperature.C-tK) + 273.150) ))
    smrA = smr * ArrheniusFactor
    mrA = mr * ArrheniusFactor

    x = data.frame( smr=smr, smrA=smrA, mr=mr, mrA=mrA )

    return(x)
  }
      
  
