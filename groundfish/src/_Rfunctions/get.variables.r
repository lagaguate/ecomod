
get.variables = function(subset) {
     
  V = switch( EXPR=subset,
  
    sp.list = c( 
      "forage.fish", "all", "allfish", "elasmobranchs", "gadoid", "flatfish",
      "demersal", "large.demersal", "small.demersal",
      "pelagic", "large.pelagic", "small.pelagic",
      "commercial", "noncommercial", 
      "cod", "haddock", "american.plaice", "silver.hake", "white.hake", 
      "capelin", "herring", "mackerel", "sandlance", "redfish", "wolffish",
      "winter.flounder", 
      "spiny.dogfish",  "thornyskate",
      "crabs", "snowcrab", "northernshrimp", "squid" 
    ), 
    multispecies = c( "all", "elasmobranchs", "demersal", "large.demersal", "small.demersal",
      "pelagic", "large.pelagic", "small.pelagic", "flatfish", "commercial", "noncommercial"
    ),
    days = c("all.1km.10day", "all.50km.10day", "all.1km.50day", "all.50km.50day" 
    ),

    all = c(
      paste( "totno", get.variables("sp.list"), sep="." ),
      paste( "totwgt", get.variables("sp.list"),  sep="." ),
      paste( "ntaxa", get.variables("multispecies"),  sep="." ),
      paste( "rmean", get.variables("sp.list"),  sep="." ),
      paste( "pmean", get.variables("sp.list"),  sep="." ),
      paste( "mmean", get.variables("sp.list"),  sep="." ),
#      paste( "lmean", get.variables("sp.list"),  sep="." ),
      paste( "nss.rsquared", get.variables("days"), sep="."),
      paste( "nss.df", get.variables("days"), sep="."),
      paste( "nss.b0", get.variables("days"), sep="."),
      paste( "nss.b1", get.variables("days"), sep="."),
      paste( "nss.shannon", get.variables("days"), sep="."),
      paste( "nss.evenness", get.variables("days"), sep="."),
      paste( "nss.Hmax", get.variables("days"), sep="."),
      paste( "ntaxa", "annual",c(1,seq(20,200,20)), sep="."),
      "C", "Z", "sar.rsq", "Npred", 
      "mr", "mrT", "smr", "smrT", "mrPvalue", "mrPvalueT",
      "ca1", "ca2", "shannon", "evenness", "Hmax",
      "sdepth", "temp", "sal", "oxyml", "julian"
    ),

    log.transform = c(
      paste( "totno", get.variables("sp.list"), sep="." ),
      paste( "totwgt", get.variables("sp.list"),  sep="." ),
      "Npred", "mr", "mrT"
    ),

    catch.summary = get.variables("sp.list") ,
  
    scaled.centered =c (""), # swtich does not like a null vector

  ) # end switch
    
  V = sort( unique(  V ) )

  return ( V )
}


