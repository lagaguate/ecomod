bio.runall = function(a, title,print=T, t0,t1)  
{
  b = bio.form( a, t0, t1 )
  bio.view( b, title, print, t0, t1)
  bio.ord( b, title, print)
}


