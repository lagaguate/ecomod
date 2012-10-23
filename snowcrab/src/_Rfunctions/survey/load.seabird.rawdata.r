
  # ---------------------------------------- low-level data access

	load.seabird.rawdata = function(filename, unique.id) {
   	
    out = NULL
    seabird=NULL
    header =  readLines(filename, n=21)
    headerall = paste( header[1:6], collapse="~")

    if (length(header) < 20)  return( out )
      
    l.study = grep( "Study ID=", header, perl=T )
    studyid = tolower( gsub("^.*Study ID=", "", header[ l.study ] ) )
    if (grepl( "test", studyid, ignore.case=T) ) return(out)
    if (grepl( "testage", studyid, ignore.case=T) ) return(out)
     
    seabird = as.data.frame(read.table( file=filename, sep=",", as.is=T, colClasses="character", header=F, skip=7))
   
    if ( nrow(seabird) < 10 ) return( out )
    
	  fileinfo = tolower(unlist(strsplit(filename, split="/")))
	  stationid = tolower(unlist(strsplit(basename(filename), "\\."))[1])
      if (grepl("^ep", stationid, ignore.case=T)) {
        stationid = gsub( "(^ep[[:space:]]*)([[:digit:]]*)([[:space:]]*.*$)", "\\2", stationid, ignore.case=T )
      } else if (grepl("asc", stationid, ignore.case=T)) {
        # older data series used different file naming conventions
        # sequence is slightly important: e.g., "post" must come before "pos"
        possible.prefixes = c( "postr", "posr", "pos t", "post", "pos", "ep", "station" )
        stationid = studyid
        for (pp in possible.prefixes ) stationid = gsub( pp, "QQQ", stationid, ignore.case=T )
        if (grepl("QQQ", stationid )) {
          stationid = gsub( "(^.*QQQ[[:space:]]*)([[:digit:]]*)([[:space:]]*.*$)", "\\2", stationid, ignore.case=T )
        }
        # manual fixes  .. still in the asc* series
        if ( gsub("[[:space:]]", "", studyid) == gsub("[[:space:]]", "", "ens 2001 zone 20 167") ) stationid = "167"
        if ( gsub("[[:space:]]", "", studyid) == gsub("[[:space:]]", "", "ens 2001 zone 20 162") ) stationid = "162"
      }
      
      if (nchar(stationid)>=1) {
        stationid = as.numeric(stationid) 
      } else {
        stationid = NA
      }
    
	  filename2 = fileinfo[length(fileinfo)] #Changed from postions in file location to the last entry since last entry is the filename
    filename2 = tolower(filename2)
	  
	  if (ncol(seabird)== 3)  seabird[,4] = NA  # fill with NA's when depth is not recorded (e.g. 1998)

    colnames(seabird) = c( "mdate", "mtime", "temperature", "depth")
    numerics = c("temperature", "depth")
    seabird = factor2number(seabird, numerics)

    # depth offets as this can be large sometimes (Esp 2009 ~ 50 m)
    surface =  mean( seabird$depth[c(1:20, (nrow(seabird)-c(1:20))) ], trim=0.1, na.rm=T)  # take from tail and head
    if ( is.finite( surface) ) seabird$depth = seabird$depth - surface
    
    # obtain date format from the seabird header
	  headerdateformat = seabirdDate( header=header, outvalue="format"  ) 
    if (is.null(headerdateformat) ) return(out)
    
    seabird$mdate = gsub("^80-", "2000-", seabird$mdate )  # there are incorrect year entries
    date.format = c( dates=headerdateformat, times="h:m:s")
    seabird$chron = chron( dates.=seabird$mdate, times.=seabird$mtime, format=date.format, out.format=dateformat.snow )
    
    yr = as.numeric( as.character( years(seabird$chron[1]) ))
    if (!is.finite(yr) ) yr = seabirdDate( header=header, outvalue="year"  ) 
    
    error = ""
    if (any(is.na(seabird$chron))) error = paste(error, "Ambiguous time format" )
    if (years(seabird$chron[1]) != yr)  error = paste(error, "Years inconsistent" )
    strangedatacheck = try( lm(temperature ~ depth, data=seabird,  na.action="na.omit"), silent=T )
    if ( "try-error" %in% class( strangedatacheck ) ) error=paste(error, "no depth data?")
    
    # error corrections:  This one is hard to fix without changing the raw data file
    if  (yr == 2006) {
      if ( is.finite(stationid) && stationid==124 ) {
        d0 = chron( dates.="2006-10-11", times.="16:37:16", format=c(dates="y-m-d", times="h:m:s"))
        d1 = chron( dates.="2006-10-16", times.="05:40:50", format=c(dates="y-m-d", times="h:m:s"))
        offset = times(d1) - times(d0)
        seabird$chron = chron( times( seabird$chron ) + offset, out.format=dateformat.snow )
    }}
   
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
 

