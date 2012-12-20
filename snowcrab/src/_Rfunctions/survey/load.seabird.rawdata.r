
  # ---------------------------------------- low-level data access

	load.seabird.rawdata = function(filename, unique.id) {
   	
    out = NULL
    seabird=NULL
    header =  readLines(filename, n=75)
    headerall = paste( header[1:12], collapse="~")

    if (length(header) < 75 )  return( out )
     
    data.start = grep( "start sample number", header, perl=T ) 
    if (grepl( "test", header[ 7 ], ignore.case=T) ) return(out)
	  
    fileinfo = tolower(unlist(strsplit(filename, split="/")))
     
    # break down file into separate stations /set no.
    seabird = as.data.frame(read.table( file=filename, sep=",", as.is=T, colClasses="character", header=F, skip= data.start, strip.white=T))
    
    if ( nrow(seabird) < 10 ) return( out )
    
    colnames(seabird) = c( "temperature", "pressure", "mdate", "mtime")
    numerics = c("temperature", "pressure")
    seabird = factor2number(seabird, numerics)
 
    # obtain date format from the seabird header
	  date.format = seabirdDate( header=header, outvalue="format"  ) 
    seabird$chron = chron( dates.=seabird$mdate, times.=seabird$mtime, format=date.format, out.format=dateformat.snow )
    
    yr = as.numeric( as.character( years( seabird$chron[1]) ))
    
    # break down multi-set records into separate records using a simple depth rule .... make sure this is in correct units (TODO)
    print (filename)
    seabird$id = decompose.into.sets( seabird$pressure )  
    seabird = seabird[ which(is.finite( seabird$id ) ) , ]
    seabird$unique_id = NA  #initiate
 
    ### NOTE :: depth, latitude and set number still need to be determined from a time-based merge of set data ... doing it here:
    set = snowcrab.db( DS="setInitial" ) 
    set$chron = string2chron(set$chron )
    set = set[ which( set$yr == yr ) ,]

    seabird$depth = NA
    
    metadata = NULL
    for ( ssid in sort( unique( seabird$id ) ) ) {
  	  stationid = "TBD" 
	    filename2 = tolower( fileinfo[length(fileinfo)] )
      latitude = NA 
      unique_id = paste( yr, unique.id, ssid, "seabird", sep=".")
      o = which( seabird$id == ssid )
      seabird$unique_id[o] = unique_id
  
      zmaxi = which.max( as.numeric( seabird$pressure[o] ) )
      if (length(zmaxi)==0) zmaxi = which.min( as.numeric( seabird$temperature[o]) )
      if (length(zmaxi)==0) zmaxi = floor( length(o) / 2 )  # take midpoint
        
      tdiff = seabird$chron[o[zmaxi]] - set$chron  # in days
      ip = which( tdiff > 0 ) # set time must be before seabird contact at bottom
      
      error = ""
      if ( length(ip) == 0 ) {
        print( "Time matching failed for seabird data .. ")
        print( filename )
        error = "No matching seabird time found" 
        studyid = gsub( "^.*Trip #:", "", header[9] )
        seabird$depth[o] = decibar2depth ( P=seabird$pressure[o], lat=44.5  ) # arbitarily fixed at mid-point of the ESS
        station = "-999"  # unknown
      } else {

        im = which.min( tdiff[ip] )
        itdiff = ip[im]
        setx =  set[itdiff,] # matching trip/set/station  
        studyid = paste( setx$trip, setx$set, setx$station, sep="." )
        seabird$depth[o] = decibar2depth ( P=seabird$pressure[o], lat=setx$lat )
        station = setx$station
      }

      out = data.frame(unique_id, yr, seabird$chron[o[zmaxi]], station, studyid, error, filename2, headerall, stringsAsFactors=F )
      names( out ) = c( "unique_id", "yr", "timestamp", "stationid", "studyid", "error", "filename", "headerall" )
      out$timestamp = as.character(  out$timestamp )

      metadata = rbind( metadata, out )
    
    }
   

    seabird$chron = as.character( seabird$chron )  # for sqlite .. chron objects convert to reals otherwise
    
    return( list( metadata=metadata, seabird.data=seabird ) )
  }
 

