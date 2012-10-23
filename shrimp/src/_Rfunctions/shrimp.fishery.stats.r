

  shrimp.fishery.stats = function( s, fleet="all", suffix="all" ) {
    fish = NULL

    if (fleet == "mobile" ) {
      s$cpue = s$weight / s$fhours
      ii = which( 
        s$btype.simple=="mobile"
        & is.finite( s$cpue) 
        & (s$fhours > 0)  
      )
      if ( length(ii) ==0)  return (fish)
      s = s[ii, ]

      l = aggregate( s$weight, list(yr=s$yr), function(x) sum(x, na.rm=T))
      names(l) = c("yr", "landings")
      l = factor2number(l, c("yr", "landings"))

      cpue = aggregate( s$cpue, list(yr=s$yr), function(x) mean(x, na.rm=T))
      names(cpue) = c("yr", "cpue")
      cpue = factor2number(cpue, c("yr", "cpue"))
   
      fish = merge( l , cpue, by=c("yr") )
      fish$effort = fish$landings / fish$cpue
      names(fish) = c("yr", paste( c("landings", "cpue", "effort"), suffix, sep="." ) )
      return (fish)
    }
    

    if (fleet == "fixed" ) {
      s$cpue = s$weight / s$ntraps
      ii = which( 
        s$btype.simple=="fixed" 
        & is.finite( s$ntraps )
        & is.finite(s$fhours) 
        & is.finite( s$cpue) 
        & s$ntraps > 0 
        & s$fhours > 0 
        & !grepl("NA", s$loc.small)  
      ) 
  
      if ( length(ii) ==0)  return (fish)
      s = s[ii,]
   
      l = aggregate( s$weight, list(yr=s$yr), function(x) sum(x, na.rm=T))
      names(l) = c("yr", "landings.fixed")
      l = factor2number(l, c("yr", "landings.fixed"))
      
      cpue = aggregate( s$cpue, list(yr=s$yr), function(x) mean(x, na.rm=T))
      names(cpue) = c("yr", "cpue.fixed")
      cpue = factor2number(cpue, c("yr", "cpue.fixed"))
      
      fish = merge( l , cpue, by=c("yr") )
      fish$effort = fish$landings.fixed / fish$cpue.fixed
      names(fish) = c("yr", paste( c("landings", "cpue", "effort"), suffix, sep="." ) )
      return (fish)
    }

    if (fleet == "all" ) { # default
      l = aggregate( s$weight, list(yr=s$yr), function(x) sum(x, na.rm=T))
      names(l) = c("yr", "landings.all")
      fish = factor2number(l, c("yr", "landings.all"))
      names(fish) = c("yr", paste( "landings", suffix, sep="." ) )
      return(fish)
    }

  }
 
	

