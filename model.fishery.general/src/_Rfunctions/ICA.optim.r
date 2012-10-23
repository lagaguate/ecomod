ICA.optim = function(inits, ... ) { 
  # simple wrapper to handle inits and error function
  ICA.result = ICA.core(inits)
  return (ICA.result$error)
}



