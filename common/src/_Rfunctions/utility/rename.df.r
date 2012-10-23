
  rename.df = function(x, n0, n1) {
    names(x)[which(names(x)==n0)] = n1
    return(x)
  }

