
time.test = function(x0, x1, threshold, type="le") {
  # take the difference between two chron objects
  require(chron)
  d = abs( as.numeric( x0) - as.numeric(x1))

  if (type=="lt") i = which(d < threshold)
  if (type=="le") i = which(d <= threshold)
  if (type=="gt") i = which(d > threshold)
  if (type=="ge") i = which(d >= threshold)
  return(i)
}


