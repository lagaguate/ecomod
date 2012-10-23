
params.weights = function(P, wBase=2, wLower=-12, wUpper=2.5, wStep=0.5) {
  # define weight bins
  P$wBase = wBase
  P$wLower = wLower
  P$wUpper = wUpper
  P$wStep = wStep
  P$w = seq( P$wLower, P$wUpper, P$wStep )
  P$wX = P$wBase^P$w
  P$wN = length(P$wX)
  return (P)
}


