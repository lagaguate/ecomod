
specific.ingestion = function(weight) {
  # obtain specific ingestion rates 
  # from Farlow (1976) quoted in Peters 1981 (in Watts/kg; pg 104)
     
  weight = weight /1000  # division by 1000 converts to kg
  Intercept = 0.78
  Slope = 0.75 -1 
  specific.ingestion = Intercept * (weight^Slope) # units = W/kg 
  specific.ingestion = specific.ingestion * 4.505143   # per year
  return (specific.ingestion)
}


