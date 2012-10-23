
find.optim.solution = function(optim.methods, vpa.model, ... ) {

  out = NULL
  for (om in optim.methods) {
    res = NULL
    if (om == "PORT" ) {
      res = nlminb( start=inits, objective=vpa.model, lower=lower, upper=upper, 
        control=list(eval.max=500, iter.max=500 ))
      res$value = res$objective
    } else if (om == "NLM-Newton" ) {
      res = nlm( p=inits, f=vpa.model)
      res$convergence = ifelse (res$code %in% c(1,2), 0, 1)
      res$value = res$minimum
    } else {
      res = optim( par=inits, fn=vpa.model, gr=NULL, method=om )
    }
    print(om)
    print(res)
    out = rbind ( out, list( 
      optim.method=om, convergence=res$convergence, error=res$value, par=res$par  # convergence if == 0
    ) ) 
    }
   return (out)
}


