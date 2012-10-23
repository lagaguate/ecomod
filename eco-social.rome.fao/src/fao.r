
  loadfunctions( c("common", "indicators") )
  

  # coplot( ns ~ year|type, data=ld )
  # coplot( ns ~ year|type, data=lv )
    
    Cairo( file="landings.landedvalues.png", width=8, height=10)
      layout( matrix( 1:2, nrow=2, ncol=1, byrow=F ))
   #   par(oma=c(4, 4, 4, 4)) # outer margins default:  c(0, 1, 0, 1)'c(bottom, left, top, right)'
   #   par(mar=c(2, 2, 2, 2))

      cols = c("green", "yellow", "orange", "blue")
      
      ld = indicators.db( db="landings.ns" )
      ldt = tapply( ld$ns, list(ld$type, ld$year), sum )
      barplot( ldt, main="Landings (metric tons)", col=cols )
   
      lv = indicators.db( db="landedvalue.ns", ref.year=2008  )
      lvt = tapply( lv$ns*1000, list(lv$type, lv$year), sum )
      barplot( lvt,  main="Landed value (CAD, inflation adjusted to 2008)", col=cols )

      legend( "topleft", legend=c("Shellfish", "Pelagic", "Other","Groundfish") , fill=cols[4:1]) 
 
    dev.off()


  # Figure 2: per capita landed value
  x11()
  lvy = indicators( db="percapita.landedvalue" )
  plot( lvy$year, lvy$percapita.landedvalue, pch=20, type="b", ylab="CAD (2008)", xlab="Year", xlim=range( lvy$year, na.rm=T)+c(-0.5, 0.5), ylim=c(0, max(lvy$percapita.landedvalue, na.rm=T)))
  
  GDP = indicators.db( db="GDP", ref.year=2008 ) 
  lines( GDP$yr, GDP$percap, lty="dotted" )
  abline( v=1992, lty="dashed")


  Pr("png", dname=".", fname="landedvalues.percapita", width=5, height=4)
 


# Figure XX .. 

  G = merge( GDP, lvy, by.x="yr", by.y="year" )
  dG = G$percapita.landedvalue-G$percap
  plot( G$yr, dG, type="l", ylim=c(0, max(dG, na.rm=T ) ))


