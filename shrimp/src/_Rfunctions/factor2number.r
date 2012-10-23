
  factor2number = function(x, vars) {
    for (i in vars) x[,i] = as.numeric(as.character(x[,i]))
    return(x)
  }


