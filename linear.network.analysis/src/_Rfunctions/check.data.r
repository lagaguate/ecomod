check.data = function(F,z) {
  # ensure steady-state assumption is correct
  
  # decompose data
  T = colSums( t(F) ) + z

  # Check Steady-State Assumption
  Tin  = colSums( t(F) ) + t(z)     # inputs
  Tout = colSums( F ) + y           # outputs
  pd = abs( (Tin-Tout) ) / Tin      # proportional difference in node throughflow

  # find number of proportional throughflow differences that are greater than
  # 0.0005 or 0.05%.
  ss_tol = 5e-3;                    # steady-state tolerance facor
  pd_count = length(which(pd>=ss_tol))  
  
  if (pd_count > 0) {
    print( "Model not at steady-state" )
  } else {
    print( "Model not at steady-state" )
  }
  return (NULL)

  }

