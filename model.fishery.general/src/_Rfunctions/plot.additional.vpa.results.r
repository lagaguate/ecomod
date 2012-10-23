
plot.additional.vpa.results = function( V ) {

  years = V$years

  a.2to3 = c(1,2) # indices of age 2 and 3
  a.4to7 = c(3,6) # indices of age 2 and 3
  
  F.2to3 = 1-exp(- colMeans( V$F[a.2to3,] ) )
  F.4to7 = 1-exp(- colMeans( V$F[a.4to7,] ) )
  yrange = range( c(F.2to3, F.4to7 ), na.rm=T )
  x11()
  plot(years, F.2to3, type="b", col="red", ylim=yrange, ylab="mortality")
  lines(years, F.4to7, type="b" )

  
  B.total = colSums(V$B,na.rm=T)
  SSB.total = colSums(V$SSB, na.rm=T)
  yrange = range( c(SSB.total, B.total, na.rm=T ) )
  x11()
  plot(years, B.total, type="b",  ylim=yrange, ylab="biomass")
  lines(years, SSB.total, type="b", col="red")

}



