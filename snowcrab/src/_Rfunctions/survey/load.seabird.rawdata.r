
  # ---------------------------------------- low-level data access

	load.seabird.rawdata = function(fn, f, set) {
   	
    out = NULL

    filename= fn[f]
    header =  readLines(filename, n=75)
    headerall = paste( header[1:12], collapse="~")

    if (length(header) < 75 )  return( out )
     
    data.start = grep( "start sample number", header, perl=T ) 
    if (grepl( "test", header[ 7 ], ignore.case=T) ) return(out)
	  
    fileinfo = tolower(unlist(strsplit(filename, split="/")))
    seabird = as.data.frame(read.table( file=filename, sep=",", as.is=T, colClasses="character", header=F, skip= data.start, strip.white=T))
    
    if ( nrow(seabird) < 10 ) return( NULL )
    
    colnames(seabird) = c( "temperature", "pressure", "mdate", "mtime")
    numerics = c("temperature", "pressure")
    seabird = factor2number(seabird, numerics)
 
    # obtain date format from the seabird header
	  date.format = seabirdDate( header=header, outvalue="format"  ) 
    seabird$chron = chron( dates.=seabird$mdate, times.=seabird$mtime, format=date.format, out.format=dateformat.snow )
 
    # check time first
    seabird.date.range = range( seabird$chron )
    setxi = which( set$chron >= seabird.date.range[1] & set$chron <= seabird.date.range[2] )
    if ( length( setxi ) == 0 ) return(NULL)
        
    yr = as.numeric( as.character( years( seabird$chron[1]) ))
    
    # First pass: 
    # break down multi-set records into separate records using a simple depth rule 
    print (filename)
    
    seabird$pressure = adjust.depth.for.drift( seabird$pressure ) 
    surface = quantile( seabird$pressure, probs=0.5 ) # pressure at which it is assumed the sensor in at surface (most data will be shallow)
    
    # initiate new variables
    seabird$seabird_uid = NA
    seabird$depth = NA
    minute = 1 / 24 / 60 # 1 minute in chron terms

    metadata = NULL
    for ( ssid in 1:length(setxi) ) {
      
	    filename2 = tolower( fileinfo[length(fileinfo)] )

      iset = setxi[ssid]
      setx =  set[iset,] # matching trip/set/station  
     
      # find sets by running until some threshold depth is found, stating with set$chron 
      istart = which.min( abs(seabird$chron - set$chron[iset] ))
      
      j0 = istart
      while ( j0 > 1 ) {
        if (seabird$pressure[j0] <= surface ) break() 
        j0 = j0 - 1
      }
      tdiff0 = abs(seabird$chron[j0] - set$chron[iset] )
      if ( tdiff0 > (10 * minute) ) { 
        # then something went wrong .. default to a few minutes before set$chron
        j0 = which.min( abs( seabird$chron - (set$chron[iset] - 5 * minute) )) 
      }

    
      j1 = istart
      while ( j1 < nrow(seabird) ) {
        if (seabird$pressure[j1] <= surface ) break() 
        j1 = j1 + 1
      }
      tdiff1 = abs(seabird$chron[j1] - set$chron[iset] )
      if ( tdiff1 > (15 * minute) ) { 
        # then something went wrong .. default to a few minutes before set$chron
        j1 = which.min( abs( seabird$chron - (set$chron[iset] + 12 * minute) )) 
      }

      o = j0:j1
      error = "" #dummy value

      if (length(o) < 30) {
        # no matching time range found ... use closest match and raise a flag
        # data stream did not start appropriately .. use minilogs
        ot = which.min( abs( seabird$chron - setx$chron) )
        o =  which( seabird$chron >= (seabird$chron[ot]- 2*minute) & 
                 seabird$chron <= (seabird$chron[ot] + 10*minute)  )  # 5 min tow + 5 min in case
        print( "No set data matched for seabird data:")
        print( filename )
        print( head( seabird[ o,] ) )
        print( "The following is the closest matching in set:")  
        print( setx)
        next()
      }

      zmaxi = which.max( as.numeric( seabird$pressure[o] ) )
      if (length(zmaxi)==0) zmaxi = which.min( as.numeric( seabird$temperature[o]) )
      if (length(zmaxi)==0) zmaxi = floor( length(o) / 2 )  # take midpoint
     
      tstamp = seabird$chron[o[zmaxi]] 
      uid =  paste( "seabird", setx$trip, setx$set, setx$station, hours(tstamp), minutes(tstamp), f, sep=".")
      seabird$seabird_uid[o] = uid
      seabird$depth[o] = decibar2depth ( P=seabird$pressure[o], lat=setx$lat )
      studyid = paste( setx$trip, setx$set, setx$station, sep="." )
      out = data.frame( uid, yr, seabird$chron[o[zmaxi]], setx$trip, setx$set, setx$station, studyid, setx$Zx, setx$chron, error, filename2, headerall, stringsAsFactors=FALSE )
      names( out ) = c( "seabird_uid", "yr", "timestamp", "trip", "set", "station", "studyid", "setZx", "setChron", "error", "filename", "headerall" ) 
      metadata = rbind( metadata, out )
    } 

    basedata = seabird[ which( !is.na( seabird$seabird_uid) ) ,]

    return( list( metadata=metadata, basedata=basedata ) )
  }
 

