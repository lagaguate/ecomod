
  data.quality.check = function( set, type ) {
    
  
    if (type=="tow.duration") {
      e0 = which( ( set$dt > 8 | set$dt < 4 )  & set$yr >=2004 )
      if  (length(e0)>0 ) {
        print( "The following have rather short/long tow times (dt)" )
        print( set[e0,] )
      }
      return (set[e0,] ) 
    }

  
    if (type=="netmind.timestamp") {
      #  check times/data and merge remaining data using datestamps and {station, set} 
      time.diff = set$netmind.timestamp - set$chron
      time.thresh = 30/60/24  # in days
      i = which( abs( as.numeric( time.diff)) > time.thresh )  
      if (length(i)>0) {
        print ("Potential date/time mismatches::")
        print( set[i, ] )
        return(set[i,])
      } 
    }
  
    # netmind mismatches
    if(type=="netmind.mismatches") {
      q = which( set$yr > 2004 & (set$netmind_uid==""| is.na(set$netmind_uid) )  )
      if ( length (q) > 0 ) {
        print( "No netmind matches for the following sets:")
        print ( set[q,c("trip", "set", "station", "t0", "chron") ] )
        return( set[q,c("trip", "set", "station", "t0", "chron") ] )
      }
    }


    # duplicated stations
    if (type=="stations") {
      must.be.unique = paste( set$yr, set$station, sep="~" )
      dups = duplicates.toremove( must.be.unique )
      x = set[ dups, c( "trip", "set", "station" ) ]
      x = x[ is.finite(x$station), ]
      dup.stations = x[ order(x$station) ,] 
      print( "Duplicated stations:" )
      print( dup.stations  )
      return (dup.stations)
    }    
       
    # poor minilog matches
    if(type=="minilog") {
      must.be.unique = set$t0
      dups = duplicates.toremove( must.be.unique )
      x = set[ dups, c( "trip", "set", "station", "t0" ) ]
      dup.t0 = x[ is.finite(x$t0), ]
      print( "Duplicated minilog times:" )
      print( dup.t0 )
      return (dup.t0)
    }
    
    # minilog mismatches
    if(type=="minilog.mismatches") {
      q = which( set$yr > 2004 & (set$minilog_uid==""| is.na(set$minilog_uid) )  )
      if ( length (q) > 0 ) {
        print( "No minilog matches for the following sets:")
        print ( set[q,c("trip", "set", "station", "t0", "chron") ] )
        return( set[q,c("trip", "set", "station", "t0", "chron") ] )
      }
    }

    if(type=="minilog.dateproblems") {
      time.thresh = 1/24  # in days
      ii = which( ( abs( as.numeric( set$t0 - set$chron)) > time.thresh ) )   
      if ( length (ii) > 0 ) {
        print( "Minilog date mismatches with Trip ID, using Trip id as default pre 2005:" )
        print ( set[ii, c("trip", "set", "station", "t0", "chron", "minilog_uid")] )
        return ( set[ii, c("trip", "set", "station", "t0", "chron", "minilog_uid")] )
      }
    }

    # positional information
    if(type=="position") {
      plot(set$lon, set$lat) 
      inside = filter.region.polygon( set[, c("lon", "lat") ], "cfaall")
      if (length (inside) == nrow(set) ) {
        print("All data are within positional bounds")
        return (NULL)
      } else {  
        outside = setdiff( c(1:nrow(set), inside) )
        points( set$lon[outside], set$lat[outside], col="red")
        print( "------------- The following are out of the cfa bounds: "  )
        print( set[ outside, ] )
        return(  set[ outside, ]  )
      }
    }

    # counts of stations by area
    if(type=="count.stations") {
      years = sort( unique( set$yr ) )
      nyears = length(years)
      nregions = length(p$regions)
      res = matrix( NA, nrow=nyears, ncol=nregions)
      for (r in 1:nregions) {
        nr = filter.region.polygon(x=set, region=recode.areas(p$regions[r]), planar=F)
        for (y in 1:nyears) {
          ni = which( set$yr==years[y] )  
          res[y,r] = length( unique( intersect (nr, ni) ) )
      }}
      x = as.data.frame(res)
      names(x) = c(p$regions)
      x$yr = years
      x = x[ , c("yr", p$regions)]
      print( "Number of stations: ")
      print (x)
      return(x)
    }

  }


