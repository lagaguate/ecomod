	
	#----------------------- Species lists
species.list = function() {
	
  gstaxa = taxa.db( "gstaxa" )

  small.pelagic = gstaxa$spec[ which( gstaxa$size=="small" & gstaxa$zone=="pelagic" ) ]
  demersal = gstaxa$spec[ which( gstaxa$zone=="demersal" ) ] 

  commercial        = gstaxa$spec[ which( gstaxa$commercial==1 )]
  noncommercial     = gstaxa$spec[ which( gstaxa$commercial==0 )]

  q = groundfish.db( "det" )
  taxa = sort( unique( q$spec))
  z=gstaxa [ which (gstaxa$spec %in% taxa ) ,]
  write.table(z, file="species.list.csv" )


  # ----------------------- Landings times-series
  indic = indicators.db( db="indicators.all" )
  small.pelagics = rowSums( indic$data[ ,c("landings.ns.pelagic.mackerel", "landings.ns.pelagic.herring", "landings.ns.pelagic.capelin")], na.rm=T ) 

  plot(  small.pelagics ~ as.numeric(names( small.pelagics )), xlim=c(1973,2005), type= "b")


  # ----------------------- supp fig x-y plots of length vs width
  q = groundfish.db( "det" )
  q$mass = log10( q$mass)  
  q$len = log10( q$len)

  x.range = range( q$len[which( is.finite(q$len) )], na.rm=T)
  y.range = range( q$mass[ which( is.finite(q$mass) )], na.rm=T )

  outdir =  file.path( competitiondir, "data", "length.width" ) 
  dir.create( outdir, recursive = T, showWarnings = F )

  cod = 10
  haddock           = 11
  silver.hake       = 14 
  halibut           = 30 
  american.plaice   = 40 
  winter.flounder   = 43 
  capelin           = 64 
  herring           = 60  
  mackerel          = 70 
  thornyskate       = 201 
  spiny.dogfish     = 220 
  sandlance         = 610 
  wolffish = c( 59, 51, 52, 50 )
  redfish           = c(20, 21, 23)

  sps = c( cod, haddock, halibut, winter.flounder, herring, silver.hake, american.plaice, mackerel, thornyskate, spiny.dogfish, sandlance ) 
  sps.name = c( "cod", "haddock", "halibut", "winter.flounder", "herring", "silver.hake", "american.plaice", "mackerel", "thornyskate", "spiny.dogfish", "sandlance" ) 
     
     .simpleCap <- function(x) {
         s <- tolower(x)
         s <- gsub(".", " ", s )
         s <- strsplit(x, " ")[[1]]
      #   if (length(s) == 1) {
      #     s <- paste( toupper(substring(s, 1,1)), substring(s, 2,1), sep="")
      #   } else {
           s <- paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
      #   }
     }

  for ( ss in 1:length(sps) ) {
    fn = file.path( outdir, paste( "mass.length.allometry", sps.name[ss], "svg", sep="." ) ) 
    r = which( q$spec %in% sps[ss] & q$sex == 1 )

    k = intersect( r, which( is.finite( q$len) & is.finite( q$mass) & is.finite( q$cf ) ) )
    R = lm( mass~len, data=q, subset=k, weights=cf )
    R.su = summary(R)
    
    text.cex = 0.75 

    Cairo( file= fn,  type="svg",  dpi=100, width=4, height=4, units="in" )
      plot(  0 ~ 0, type="n", 
        ylab=expression( paste( "Body mass ( kg; ", log[10], ")" )),
        xlab=expression( paste( "Body length ( cm; ", log[10], ")")), xlim=x.range, ylim=y.range 
      )
      points(  mass ~ len, data=q, subset=r, pch=20, cex=0.3 )
      abline( reg=R, xlim=c(1,2.2), col="black" ) 
      text( 0.1, 1.6, .simpleCap( sps.name[ss]), adj=c(0,0), cex=text.cex )
      text( 0.1, 1.3, paste( "R-squared =", round(R.su$r.squared,3) ), adj=c(0,0), cex=text.cex )
      text( 0.1, 1.0, paste( "n =", length( R.su$residuals ) ), adj=c(0,0), cex=text.cex  )
      text( 0.1, 0.7, paste( "Intercept=", signif( R.su$coefficients[1], 3 )), adj=c(0,0), cex=text.cex   )
      text( 0.1, 0.4, paste( "Slope=", signif(  R.su$coefficients[2], 3 )), adj=c(0,0), cex=text.cex   )

    dev.off()
    
    print(fn)
  }

  
}


