
  make.random.string = function(x) {
    rand.seq = floor(runif(1)*10^9)
    out = paste( x, rand.seq, sep=".")
    return(out)
  }


