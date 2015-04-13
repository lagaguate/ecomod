

load.scanmar.rawdata = function( fn, tzone="UTC", yr=NULL ) {
  # Scanmar is always UTC!!!
  scanmar=NULL
  header = readLines(fn, n=10, encoding="UTF-8", skipNul=TRUE)
  datestring = basename(fn)
 
  yr_test = substring( datestring, 1, 4 )
  if (yr_test == as.character(yr)) {
    # for 1990 to 2014 .. naming method is consistent
    mon = substring( datestring, 6, 8 )
    day = substring( datestring, 9, 10 )
  } else {
    # more info added to name in 2015 ... altered parsing
    ds = unlist( strsplit( datestring, "-") )
    mon = substring( ds[2], 1, 3 )
    day = gsub( mon, "", ds[2] )
  }

  # extract timestamp of "Start Set" --- these are not always reliable .. ignore
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
  tmp = tmp[-c(1:5)] # headers sometimes long
  
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
  
  scanmar$timestamp= paste(yr, mon, day, scanmar$time, sep="-" )
  scanmar$timestamp=gsub(":","-",scanmar$timestamp)
  scanmar$timestamp = lubridate::ymd_hms(scanmar$timestamp) # Scanmar is always UTC!!! .. which is the default of ymd_hms
  
  scanmar$netmensurationfilename = basename(fn)
  
  test = timestamp.fix ( scanmar$timestamp, threshold.hrs=2 )
  if (!is.null(test))  scanmar$timestamp = test

  scanmar = look.for.multiple.sets ( scanmar )
  scanmar$id = NA  # filled in below if from 2015 and onwards, otherwise this will be filled in later once the position/time is matched to gsinf
  
  if (yr >= 2015) {
    h1 = unlist( strsplit( header[1], "[[:punct:]]" ))
    mission = substring( h1[1], nchar(h1[1])-9, nchar(h1[1]) )
    set = as.character( h1[2] )
    header.id = paste( mission, set, sep=".") 

    f0 = unlist( strsplit( basename( fn), "[[:punct:]]" )) 
    filename.id = paste( f0[1], f0[2], sep=".")
    
    scanmar$id = header.id

    if (header.id != filename.id){ 
      print( "Header and filename do not indicate the same mission & set" )
      print( "Using header id over filename ... please check the file." )
    }
  }
  
  return(scanmar)

}



