
  copy.data.structure =function( x) {
    if (class(x) == "data.frame") {
      y = x[ 1, ]
      y = y[-1, ]
      return (y)
    }
  }

