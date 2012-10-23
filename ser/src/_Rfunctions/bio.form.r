bio.form = function (a, t0, t1)
{ 
  b = a[a$yr >= t0 & a$yr <= t1 ,]
  rownames(b) = b$yr
  b$yr = NULL
  b = scale(b, center=T, scale=T)
  return (b)
}  


