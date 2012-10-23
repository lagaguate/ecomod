
# -----------------------
run.simulation = function(P) {
  # main wrapping function ...
  gc()
  out = switch(P$runtype,
      stochastic = simul.stochastic(P),
      nonstochastic = simul.nonstochastic(P)
      
  )
  return(out)
}


