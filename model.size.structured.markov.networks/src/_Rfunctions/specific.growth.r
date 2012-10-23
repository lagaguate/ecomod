
specific.growth = function(weight) {  
  # obtain specific growth rates from Farlow (1976), reanalysed by Peters 1981 (in Watts/kg; pg 143, Table 8.5 )
  weight = weight /1000  # division by 1000 converts to kg
  Intercept = 0.16
  Slope = 0.75 - 1
  specific.growth = Intercept * (weight^Slope)
  specific.growth = specific.growth * 4.505143  # convert from Watts to year-1 
  return (specific.growth)
}
 

