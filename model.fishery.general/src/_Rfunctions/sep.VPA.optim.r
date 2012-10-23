sep.VPA.optim = function(inits, ... ) {
  # simple wrapper to handle inits and error function
  sep.vpa.result = sep.VPA.core(inits)
  return (sep.vpa.result$error)
}


