#// Draw Richard's Selectivity Curve
RichardsCurve =  function(param, x=1:200){
  
  y = (exp(param[1]+param[2]*x)/(1+exp(param[1] + param[2] * x)))^(1./param[3])
  
  L50 = (log(0.5^param[3]/(1-0.5^param[3]))-param[1])/param[2]; # 50% selectivity

  return(list(x=x, y=y, L50=L50))

  } 

