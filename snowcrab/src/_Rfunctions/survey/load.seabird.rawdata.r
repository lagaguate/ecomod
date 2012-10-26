
  # ---------------------------------------- low-level data access

	load.seabird.rawdata = function(filename, unique.id) {
   	
    out = NULL
    seabird=NULL
    header =  readLines(filename, n=75)
    headerall = paste( header[1:12], collapse="~")

    if (length(header) < 75 )  return( out )
     
    data.start = grep( "start sample number", header, perl=T ) 
    studyid = header[ 7 ] 
    if (grepl( "test", studyid, ignore.case=T) ) return(out)
	  
    fileinfo = tolower(unlist(strsplit(filename, split="/")))
     
    # break down file into separate stations /set no.
    seabird = as.data.frame(read.table( file=filename, sep=",", as.is=T, colClasses="character", 
      header=F, skip= data.start, strip.white=T))
    
    if ( nrow(seabird) < 10 ) return( out )
    
    seabird = seabird.decompose.into.sets( seabird, threshold.depth=10 )  # break down multi-set records into separate records using a simple depth rule .... make sure this is in correct units (TODO)

    # --- TODO -- incomplete to here 

	  stationid = toupper(unlist(strsplit(basename(filename), "\\."))[1])
      if (grepl("^ep", stationid, ignore.case=T)) {
        stationid = gsub( "(^ep[[:space:]]*)([[:digit:]]*)([[:space:]]*.*$)", "\\2", stationid, ignore.case=T )
      } else if (grepl("asc", stationid, ignore.case=T)) {
        stationid = gsub( "(^[[:space:]]*)", "", stationid, ignore.case=T )
      }
      
      if (nchar(stationid)>=1) {
        stationid = as.numeric(stationid) 
      } else {
        stationid = NA
      }
    
	  filename2 = fileinfo[length(fileinfo)] #Changed from postions in file location to the last entry since last entry is the filename
    filename2 = tolower(filename2)
	  
    colnames(seabird) = c( "temperature", "depth", "mdate", "mtime")
    numerics = c("temperature", "depth")
    seabird = factor2number(seabird, numerics)

    # depth offets as this can be large sometimes (Esp 2009 ~ 50 m)
    surface =  mean( seabird$depth[c(1:10) ], trim=0.1, na.rm=T)  
    if ( is.finite( surface) ) seabird$depth = seabird$depth - surface
    
    # obtain date format from the seabird header
	  date.format = seabirdDate( header=header, outvalue="format"  ) 
    seabird$chron = chron( dates.=seabird$mdate, times.=seabird$mtime, format=date.format, out.format=dateformat.snow )
    
    yr = from header .. filename or data blob.
    if (!is.finite(yr) ) yr = seabirdDate( header=header, outvalue="year"  ) 
    
    error = ""
    if (any(is.na(seabird$chron))) error = paste(error, "Ambiguous time format" )
    if (as.numeric(as.character(years(seabird$chron[1]))) != yr)  error = paste(error, "Years inconsistent" )
   
    seabird$chron = as.character( seabird$chron )  # for sqlite .. chron objects convert to reals otherwise
    unique.id = paste( yr, unique.id, "seabird", sep=".")
    seabird$unique_id = unique.id
    
    zmaxi = which.max( as.numeric( seabird$depth) )
    if (length(zmaxi)==0) zmaxi = which.min( as.numeric( seabird$temperature) )
    if (length(zmaxi)==0) zmaxi = floor( nrow(seabird) / 2 )  # take midpoint
    if ( !(length(zmaxi)==1) ) stop( filename )
        
    out = data.frame(unique.id, yr, seabird$chron[zmaxi], stationid, studyid, error, filename2, headerall)
    names( out ) = c( "unique_id", "yr", "timestamp", "stationid", "studyid", "error", "filename", "headerall" )
    
    return( list( metadata=out, seabird.data=seabird ) )
  }
 

