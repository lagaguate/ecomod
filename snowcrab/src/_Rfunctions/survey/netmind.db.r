

  netmind.db = function( DS, Y=NULL ) {

    iY = which( Y>=1999 )  # no historical data prior to 1998
    if (length(iY)==0) return ("No data for specified years")
    Y = Y[iY]

    if (DS=="load") {
      # this safely replaces any updates
      if (any( Y < 2004) ) {
        print( "Net metrics and bottom contact stats were processed manually by Gulf Region until 2004 and now stored in 'SNTOWS'" )
      }

      dirlist = list.files(path=netmind.rawdata.location, full.names=T, recursive=T)
      badtows = grep("bad", tolower(dirlist) , ignore.case=T )
      if (length(badtows) >0) dirlist = dirlist[ -badtows ]  # remove tows with "bad" in the filename

      nfiles = length(dirlist)
      filelist = matrix( NA, ncol=3, nrow=nfiles)
      for (f in 1:nfiles) {
        yr = netmindDate( fnNetmind=dirlist[f] )
        if ( is.null(yr) ) next()
        if ( yr %in% Y ) filelist[f,] = c( f, dirlist[f], yr )
      }
      filelist = filelist[ which( !is.na( filelist[,1] ) ) , ]

      con <- dbConnect( dbDriver("SQLite"), nDB)
      dbSendQuery( con, " PRAGMA synchronous=OFF ;" ) # speeds up writes using buffers ... but no longer atomic
      for ( yr in Y ) {
        print(yr)
        netmind.delete(con, yr) # remove any data match the year that is being added
        fs = filelist[ which( as.numeric(filelist[,3])==yr ) , 2 ]
        if (length(fs)==0) next()
        for (f in 1:length(fs) ) {
          j = load.netmind.rawdata( fs[f], unique.id=f )  # variable naming conventions in the past
          if (is.null(j)) next()
          dbWriteTable(con, nMeta, j$metadata, overwrite=F, row.names=F, append=T)
          dbWriteTable(con, nBase, j$basedata, overwrite=F, row.names=F, append=T)
        }
      }
      dbDisconnect(con)
      return ( nDB )
    }


    if (DS %in% c("metadata") ){  # dump all metadata
      con = dbConnect( dbDriver("SQLite"), nDB)
      yrs = paste(Y, collapse=",")
      M = dbGetQuery(con, paste( "SELECT * FROM ", nMeta, " WHERE yr IN (", yrs, ");", sep="" ) )
      dbDisconnect(con)
      return(M)
    }

    if (DS %in% c("basedata") ){
      con = dbConnect( dbDriver("SQLite"), nDB)
      yrs = paste(Y, collapse=",")
      M = dbGetQuery(con, paste( "SELECT * FROM ", nBase, " WHERE yr IN (", yrs, ");", sep="" ) )
      dbDisconnect(con)
      return(M)
    }


    if (DS=="stats.redo") {
      set = snowcrab.db( DS="set.minilog.netmind" ) # a view specific to this step only
      con = dbConnect( dbDriver("SQLite"), nDB)
      dbSendQuery( con, " PRAGMA synchronous=OFF ;" ) # speeds up writes using buffers ... but no longer atomic
      tableExists = dbExistsTable( con, nStats )
        if ( ! tableExists ) { # initialize with the right data formats
            O = data.frame(slon=0)
            O$slon=0.01
            O$slat=0.01
            O$distance=0.01
            O$spread=0.01
            O$spread_sd=0.01
            O$surfacearea=0.01
            O$vel=0.01
            O$vel_sd=0.01
            O$netmind_n=0.01
            O$t0=" "
            O$t1=" "
            O$dt=" "
            O$yr=0.01
            O$unique_id="dummy"
          dbWriteTable(con, nStats, O, overwrite=T, row.names=F, append=F)
        }

      for ( yr in Y ) {
        print(yr)
        rid = dbGetQuery(con, paste( "SELECT unique_id FROM ", nMeta, " WHERE yr=", yr, ";", sep="" ) )
        if (nrow(rid) == 0 ) next()
        if ( tableExists ) dbSendPreparedQuery( con, paste( "DELETE FROM ", nStats, " WHERE unique_id=:unique_id ;", sep=""),  rid )
        out = NULL
        for ( id in rid$unique_id ) {
          N = dbGetQuery(con, paste( "SELECT * FROM ", nBase, " WHERE unique_id='", id, "'", sep="" ) )
          iS = which( set$netmind_uid == id )
          if (length(iS) == 1 ) {
            l = net.configuration( N, t0=set$t0[iS], t1=set$t1[iS], tchron=set$chron[iS] )
          } else {
            l = net.configuration( N )
          }
          l$unique_id = id
          out = rbind( out,l )
        }
        # clean data from a particular year before appending
        if ( dbExistsTable(con, nStats )) {
          dbSendQuery( con, paste( "DELETE FROM ", nStats, " WHERE yr=", yr, sep="") )
        }
        dbWriteTable(con, nStats, out, overwrite=F, row.names=F, append=T)
      }
      dbDisconnect(con)
      return ( nDB )
    }


    if (DS == "stats" ) {
      con = dbConnect( dbDriver("SQLite"), nDB)
      yrs = paste(Y, collapse=",")
      qry = paste( "SELECT m.trip, m.setno as 'set', m.station, s.* ",
              " FROM ", nMeta, " m  JOIN ", nStats, " s",
              " ON m.unique_id=s.unique_id",
              " WHERE m.yr IN (", yrs, ");"
          , sep="" )
      res = dbGetQuery(con, qry )
      dbDisconnect(con)
      res$netmind_uid = res$unique_id
      res$t0n = string2chron( res$t0 )
      res$t1n = string2chron( res$t1 )
      dt = res$dt
      res$dt = NA
      i = which(!is.na( dt ) )
      if (length(i) >0 ) res$dt[i] = times( dt[i] )
      res$unique_uid = res$t0 = res$t1 = NULL
      return (res)
    }


    if (DS=="set.netmind.lookuptable" ) {
      # must fix:: individuala record updates or yearly updates slow and not removing all relevent records
      # For now just refresh the whole table

      set = sqlite.read ( db.snow, "setInitial" )
      set$chron = string2chron ( set$chron )
      set$netmind_uid = ""  # this is the linking field that will be created in this step
      nset = nrow( set )
      Y = sort(unique(set$yr))

      con = dbConnect(dbDriver("SQLite"), nDB )
      dbGetQuery( con, paste("ATTACH '", db.snow, "' AS netmind", sep="") )

      N = netmind.db( DS="metadata", Y )
      N$netmind_uid = N$unique_id
      N$unique_id = NULL
      N$netmind_timestamp = string2chron ( N$netmind_timestamp )

      # primary matching -- priority when all recorded primary fields match
      for (i in 1:nset)  {
        iii = which( N$trip==set$trip[i] & N$setno==set$set[i] & N$station==set$station[i]  )
        iii = iii[ which( is.finite( iii) ) ]
        iii = unique( iii )
        niii = length( iii )
        # accept only unique matches
        if ( niii == 1) set$netmind_uid[i] = N[ iii , "netmind_uid" ]
      }

      # time only (short time scale ~ exact time-based matching)
      for (DT in c(5, 10, 20, 30)/60/24 ) {  # time thresholds to consider matching, in minutes
        noid = which( set$netmind_uid =="" )
        if (length(noid)==0 ) next()
        for (i in noid)  {
          dt = abs( as.numeric( N$netmind_timestamp - set$chron[i]) )
          iii = which( dt < DT  )
          iii = iii[ which( is.finite( iii) ) ]
          iii = unique( iii )
          niii = length( iii )
          # accept only unique matches
          if ( niii == 1 ) { # add only if the id is not used
            if (!  any(grepl( N[ iii , "netmind_uid" ], set$netmind_uid, ignore.case=T ) )) {
              #   print ( N[iii,] )
              set$netmind_uid[ i ] = N[ iii , "netmind_uid" ]
        }}}

        noid = which( set$netmind_uid =="" )
        if (length(noid)==0 ) next()
        for (i in noid)  {
          dt = abs( as.numeric( N$netmind_timestamp - set$chron[i]) )
          iii = which( dt < DT &  N$station==set$station[i]   )
          iii = iii[ which( is.finite( iii) ) ]
          iii = unique( iii )
          niii = length( iii )
          # accept only unique matches
          if ( niii == 1 ) { # add only if the id is not used
            if (!  any(grepl( N[ iii , "netmind_uid" ], set$netmind_uid, ignore.case=T ) )) {
              #   print ( N[iii,] )
              set$netmind_uid[ i ] = N[ iii , "netmind_uid" ]
        }}}

      }


      ### the following are error checks to ID those with Potential data entry errors
      # no matches (which is good as the above is sufficient, but in case there are errors in time stamps)
      noid = which( set$netmind_uid =="" )
      if (length(noid)> 0 ) {
        for (i in noid)  {
          iii = which( N$trip==set$trip[i]  & N$setno==set$set[i]  )
          iii = iii[ which( is.finite( iii) ) ]
          iii = unique( iii )
          niii = length( iii )
          if ( niii > 0  ) {
            if (!  any(grepl( N[ iii , "netmind_uid" ], set$netmind_uid, ignore.case=T ) )) {
              print ("Potential time stamp errors? matches positive on trip and set but not time -- check this" )
              print ( N[iii,] )
              print ( set[i,]  )
      }}}}

      # no matches (which is good as the above is sufficient, but in case there are errors in time stamps)
      noid = which( set$netmind_uid =="" )
      if (length(noid)> 0 ) {
        for (i in noid)  {
          iii = which( N$trip==set$trip[i]  & N$station==set$station[i]  )
          iii = iii[ which( is.finite( iii) ) ]
          iii = unique( iii )
          niii = length( iii )
          if ( niii > 0 ) {
            if (!  any(grepl( N[ iii , "netmind_uid" ], set$netmind_uid, ignore.case=T ) )) {
              print ("Potential time stamp errors? Matches positive on trip and station -- check this" )
              print ( N[iii,] )
              print ( set[i,]  )
      }}}}

      # 1 match (which is good as the above is sufficient, but in case there are errors in time stamps)
      noid = which( set$netmind_uid =="" )
      if (length(noid)> 0 ) {
        for (i in noid)  {
          iii = which( N$yr==set$yr[i]  & N$station==set$station[i]  )
          iii = iii[ which( is.finite( iii) ) ]
          iii = unique( iii )
          niii = length( iii )
          # accept only unique matches
          if ( niii == 1 ) { # add only if the id is not used
            if (!  any(grepl( N[ iii , "netmind_uid" ], set$netmind_uid, ignore.case=T ) )) {
              print ("Potential time stamp errors or station id errors? Matches positive on year and station -- not used must check" )
              print ( N[iii,] )
              print( set[i,] )
              print( "trip=S10092006, set=1 -- netmind data file is missing the one that is here is for a bad tow:: deleted the wrong one? -- check with Ben" )
              # set$netmind_uid[ i ] = N[ iii , "netmind_uid" ]
              #
      }}}}

      #  1 matches on day and station number (longer time scale) -- they check out OK -- accepted
      DT = 1/60/24 # threshold no days to allow matches
      noid = which( set$netmind_uid =="" )
      if (length(noid)> 0 ) {
        for (i in noid)  {
          dt = abs( as.numeric( N$netmind_timestamp - set$chron[i] ) )
          iii = which( dt < DT &  N$station==set$station[i]  )
          iii = iii[ which( is.finite( iii) ) ]
          iii = unique( iii )
          niii = length( iii )
          if ( niii == 1 ) { # add only if the id is not used
            if (!  any(grepl( N[ iii , "netmind_uid" ], set$netmind_uid, ignore.case=T ) )) {

              # the following are verified to be correct: add to the list of exceptions
              if ( N$station[iii] ==136 & N$yr[iii]==2001 ) {
                set$netmind_uid[ i ] = N[ iii , "netmind_uid" ] # error caused by N$trip is filled with nonsense
                next()
              }

              print ("Potential time stamp errors or station id errors? " )
              print( "Matches positive on day of survy and station but not shorter time intervals -- not used" )
              print ( N[iii,] )
              print ( set[i,] )
      }}}}

      set = set[, c("trip", "set", "netmind_uid" )]

      # if (dbExistsTable(con, setNetmindLookup)) dbSendQuery( con, paste( "DROP TABLE ", setNetmindLookup, ";" ) )
      # Trying to do this on a record by record basis seems to cause loose ends .. redo who whole table for now
      dbWriteTable(con, setNetmindLookup, set, overwrite=T, append=F, row.names =F )
      dbDisconnect(con)

    }


  }


