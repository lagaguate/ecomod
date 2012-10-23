bio.flatten = function(a, t0, t1)
{
  b = bio.form(a, t0, t1)
  corel = cor(t(b), use="pairwise.complete.obs")  
  tri = lower.tri(corel,diag=F) 
  tri[!tri] = NA
  Z = tri * corel
  flat = as.vector(Z)
  return (flat) 
}  


