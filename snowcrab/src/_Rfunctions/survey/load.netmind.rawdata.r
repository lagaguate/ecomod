
    load.netmind.rawdata = function(fn, unique.id) {

      out=NULL
      netmind=NULL
      header = readLines(fn, n=20)
      outfmt = c(dates="year-m-d", times="h:m:s")

      if (length(header) < 20 ) return( out )
      localtime = netmindDate( header=header, outvalue="localtime" )

      tmpfile = "tmp.netmind"

      # remove "*" -- hard to do internally without fileswapping out and reloading
      tmp = readLines(fn)
      tmp = gsub("[*]", "", tmp )
      write( tmp, file=tmpfile )

      # skip 16 because first few records are sometimes incomplete
      netmind = read.table( file=tmpfile, sep="", as.is=T, colClasses="character", header=F, skip=16)
      file.remove(tmpfile)
      nr0 = nrow(netmind)
      if (nr0 < 30 ) return(out)

      nc0 = ncol(netmind)
      if ( nc0 < 12 ) return ( out ) # no net metrics stored ( only position and speed )  -- ignored
      if ( nc0 > 14 ) stop( fn)  # should not be the case unless new data streams start
      if ( nc0 == 12) {
        # older files do not have depths, add a dummy column
        # more modern data have 13 columns 2000 +
        netmind$depth= NA
      }
      if ( nc0 == 14) {
        # a few files have wing spread as well
        # more modern data have 13 columns 2000 +
        netmind[,13] = NULL
      }

      colnames(netmind) = c("ndate", "ntime", "lat.deg", "lat.min", "lat.orient",
                            "lon.deg", "lon.min", "lon.orient", "speed", "primary", "secondary", "doorspread", "depth")

      numbers = c("lat.deg", "lat.min", "lon.deg", "lon.min", "speed", "primary", "secondary", "doorspread", "depth")
      netmind = factor2number(netmind, numbers)

      netmind$tinc = 1:nrow(netmind)

      # determine deepest point if possible, using a smoothed depth as variability due to incorrect pings are frequent
      deepest.point = NULL
      if (nc0 %in% c( 13, 14 ) ) {
        require(mgcv)
        z.gam = try ( gam( depth ~ s(tinc, k=5, bs="ts"), data=netmind, optimizer=c("outer", "nlm") ), silent=T )
        if ( ! "try-error" %in% class( z.gam )) {
          netmind$depth.smoothed = predict( z.gam, newdata=netmind, newdata.guaranteed=T  )
          deepest.point = which.max ( netmind$depth.smoothed)
        }
      }

      if ( nc0==12 | length(deepest.point) == 0 ) deepest.point = round( nrow(netmind) / 2 )

      netmind$lon = - (netmind$lon.deg + (netmind$lon.min / 60) )
      netmind$lat =    netmind$lat.deg + (netmind$lat.min / 60)
      netmind = netmind[, c("ndate", "ntime", "lat", "lon", "speed", "primary", "secondary", "doorspread", "depth")]
      netmind$ndate = paste(substring(netmind$ndate,1,2), substring(netmind$ndate,3,4), substring(netmind$ndate,5,6), sep="-")
      netmind$ntime = paste(substring(netmind$ntime,1,2), substring(netmind$ntime,3,4), substring(netmind$ntime,5,6), sep=":")

      netmind$chrono = chron( dates.=netmind$ndate, times.=netmind$ntime, format=c(dates="y-m-d", times="h:m:s"), out.format=outfmt )

      # netmind data stored in GMT from GPS; the offset varies depending upon season due to daylight savings time (3 or 4 hrs)
      # obtain time offset in hours
      time.offset = netmindDate( header=header, outvalue="timeoffset" )  / 24 #  convert hours to fractional days
      netmind$chrono = as.chron( as.numeric(netmind$chrono) + time.offset, out.format = outfmt )

      netmind.timestamp = netmind$chrono[deepest.point]
      yr = as.numeric( as.character( years( netmind.timestamp ) ) )
      unique.id = paste( yr, unique.id, "netmind", sep=".")

      filename = basename(fn)

      line.localtime = grep("Local Time:", header, ignore.case=T  )
      line.ship = grep("Ship:", header, ignore.case=T  )
      line.comments = grep("Comments:", header, ignore.case=T )


      trip = gsub( "^.*Trip:", "", header[ line.ship ] )
      trip = gsub( "Tow:.*$", "", trip )
      trip = gsub( "[[:space:]]", "", trip )

      if ( ! grepl( "^S[[:digit:]]{8}$", trip, ignore.case=T ) )  { # not a standard code
        dy = paste( "00", as.character( days(netmind.timestamp) ), sep="")
        dy = substring( dy, nchar(dy)-1, nchar(dy) )
        mn = paste( "00", as.character( as.numeric(months(netmind.timestamp))), sep="")
        mn = substring( mn, nchar(mn)-1, nchar(mn) )
        yr = paste( "00", as.character( years(netmind.timestamp) ), sep="")
        yr = substring( yr, nchar(yr)-3, nchar(yr) )
        trip=paste("S", dy, mn, yr, sep="" )
      }

      setno = gsub( "^.*Tow:", "", header[ line.ship ] )
      setno = gsub( "[[:space:]]", "", setno )
      setno =  as.numeric( setno )

      station = gsub("\\..*$", "", filename) # get rid of file extensions
      station = gsub("^postr", "", station, ignore.case=T ) # must come before "r" which in some years are followed by repeat number
      station = gsub("^posr", "", station, ignore.case=T ) # must come before "r" which in some years are followed by repeat number
      station = gsub("^pos", "", station, ignore.case=T )
      station = gsub("^ep", "", station, ignore.case=T )
      station = gsub("r.*$", "", station, ignore.case=T  )
      station = gsub("_.*$", "", station, ignore.case=T  )
      station = gsub("bad.*$", "", station, ignore.case=T  )
      station = gsub("#.*$", "", station, ignore.case=T  )
      station = gsub("[[:alpha:]]", "", station)
      station = as.numeric( station )

      comments = gsub("^Comments: ", "", header[ line.comments] )

      netmind$unique.id = unique.id
      netmind$chrono = as.character( netmind$chrono )
      netmind.timestamp = as.character( netmind.timestamp )

      metadata = data.frame( filename, unique.id, yr, netmind.timestamp, trip, setno, station, comments, stringsAsFactors =F )
      names( metadata ) = c("filename", "unique_id", "yr", "netmind_timestamp", "trip", "setno", "station", "comments" )
      metadata$yr = as.numeric( as.character( metadata$yr ))

      out = list( metadata=metadata, basedata=netmind )

      return(out)
    }
