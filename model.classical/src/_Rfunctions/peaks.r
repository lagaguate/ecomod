
  peaks = function(x) {
    l = length(x)
    xm1 = c(x[-1], x[l])
    xp1 = c(x[1], x[-l])
    x[x > xm1 & x > xp1 | x < xm1 & x < xp1]
  }


