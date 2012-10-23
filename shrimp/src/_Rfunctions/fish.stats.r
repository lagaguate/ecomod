
  
  fish.stats = function (sm, type="lonlat" ) { 
    # same as aggregate.fish.stats but more options... should one of these version 
    # w is a time metric
    # lon, lat, w , catch , effort and cpue are expected in sm
    if (type=="lonlat" ) {
      l = aggregate( sm$catch, list(w=sm$w, lon=sm$lon, lat=sm$lat), function(x) sum(x, na.rm=T))
      names(l) = c("w", "lon", "lat", "catch")
      l = factor2number(l, c("w", "lon", "lat", "catch"))
      rownames(l) = NULL
      l$id = paste( l$w, l$lon, l$lat, sep="~" )
      
      e = aggregate( sm$effort, list(w=sm$w, lon=sm$lon, lat=sm$lat), function(x) sum(x, na.rm=T))
      names(e) = c("w", "lon", "lat", "effort")
      e = factor2number(e, c("w", "lon", "lat", "effort"))
      rownames(e) = NULL
      e$id = paste( e$w, e$lon, e$lat, sep="~" )
        
      cc = aggregate( sm$cpue, list(w=sm$w, lon=sm$lon, lat=sm$lat), function(x) mean(x, na.rm=T))
      names(cc) = c("w", "lon", "lat", "cpue")
      cc = factor2number(cc, c("w", "lon", "lat", "cpue"))
      rownames(cc) = NULL
      cc$id = paste( cc$w, cc$lon, cc$lat, sep="~" )
    
      x = merge( l, e, by="id", sort=F, suffixes=c("",".e") )
      x = merge( x, cc,  by="id", sort=F, suffixes=c("",".cc") )
      x = x[ , c("w", "lon", "lat", "catch", "effort", "cpue") ]
      return (x)
    }
    
    if (type=="planar" ) {
      l = aggregate( sm$catch, list(w=sm$w, plon=sm$plon, plat=sm$plat), function(x) sum(x, na.rm=T))
      names(l) = c("w", "plon", "plat", "catch")
      l = factor2number(l, c("w", "plon", "plat", "catch"))
      rownames(l) = NULL
      l$id = paste( l$w, l$plon, l$plat, sep="~" )
      
      e = aggregate( sm$effort, list(w=sm$w, plon=sm$plon, plat=sm$plat), function(x) sum(x, na.rm=T))
      names(e) = c("w", "plon", "plat", "effort")
      e = factor2number(e, c("w", "plon", "plat", "effort"))
      rownames(e) = NULL
      e$id = paste( e$w, e$plon, e$plat, sep="~" )
        
      cc = aggregate( sm$cpue, list(w=sm$w, plon=sm$plon, plat=sm$plat), function(x) mean(x, na.rm=T))
      names(cc) = c("w", "plon", "plat", "cpue")
      cc = factor2number(cc, c("w", "plon", "plat", "cpue"))
      rownames(cc) = NULL
      cc$id = paste( cc$w, cc$plon, cc$plat, sep="~" )
    
      x = merge( l, e, by="id", sort=F, suffixes=c("",".e") )
      x = merge( x, cc,  by="id", sort=F, suffixes=c("",".cc") )
      x = x[ , c("w", "plon", "plat", "catch", "effort", "cpue") ]
      return (x)
    } 

  }
 
