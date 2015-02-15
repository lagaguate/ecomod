

load.scanmar.rawdata = function( fn, tzone="America/Halifax" ) {
  
  scanmar=NULL
   
  header = readLines(fn, n=10, encoding="UTF-8", skipNul=TRUE)
  datestring = basename(fn)
  yr = substring( datestring, 1, 4 )
  mon = substring( datestring, 6, 8 )
  day = substring( datestring, 9, 10 )
  
  # extract timestamp of "Start Set"
  # line.start = grep("Start Set", header, ignore.case=T  )
    
  # timestring = header[ line.start]
  # time = substring(timestring, 1, 8)
  
  if (length(header) < 9 ) return( NULL )
   
  tmpfile = file.path (getwd(), "tmp.scanmar")
  # remove "*" -- hard to do internally without fileswapping out and reloading
  tmp = readLines(fn, encoding="UTF-8", skipNul=TRUE)
  u = nchar(tmp)
  v = which(u>40)
  if (length(v)<100) {return(NULL)}
    
                      
  tmp = tmp[v]
  write( tmp, file=tmpfile )
  
  # skip 16 because first few records are sometimes incomplete
  scanmar = read.table( file=tmpfile, sep="", as.is=T, colClasses="character", header=F)
  file.remove(tmpfile)
  nr0 = nrow(scanmar)
  if (nr0 < 30 ) return(NULL)
  
  # nc0 = ncol(scanmar)
  # if ( nc0 < 12 ) return ( out ) # no net metrics stored ( only position and speed )  -- ignored
  # if ( nc0 > 14 ) stop( fn)  # should not be the case unless new data streams start
  #if ( nc0 == 12) {
    # older files do not have depths, add a dummy column
    # more modern data have 13 columns 2000 +
   # scanmar$depth= NA
  #}
   
  colnames(scanmar) = c("time", "depth", "doorspread", "wingspread", "opening", "clearance",
                        "ltspeed", "gyro", "latitude", "longitude")
  
  
  numbers = c("depth", "doorspread", "wingspread", "opening", "clearance",
              "ltspeed", "gyro", "latitude", "longitude")
  
  scanmar = factor2number(scanmar, numbers)
  

  scanmar$doorspread = filter.nets("doorspread.range", scanmar$doorspread)
  scanmar$wingspread = filter.nets("wingspread.range", scanmar$wingspread)
  scanmar$clearance = filter.nets("clearance.range", scanmar$clearance)
  scanmar$opening = filter.nets("opening.range", scanmar$opening)
  scanmar$depth = filter.nets("depth.range", scanmar$depth)
  
  scanmar$timestamp= paste(yr,mon, day, scanmar$time, sep="-" )
  scanmar$timestamp=gsub(":","-",scanmar$timestamp)
  scanmar$timestamp = ymd_hms(scanmar$timestamp, tz=tzone ) 
  scanmar$id = basename(fn)
  
  test = timestamp.fix ( scanmar$timestamp, threshold.hrs=2 )
  if (!is.null(test))  scanmar$timestamp = test

  return(scanmar)
}

