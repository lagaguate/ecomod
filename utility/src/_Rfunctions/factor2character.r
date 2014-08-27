
  factor2character = function(x, vars) {
    for (i in vars) x[,i] = as.character(x[,i])
    return (x)
  }


