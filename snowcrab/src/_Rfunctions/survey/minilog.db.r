	

  minilog.db = function( DS="", Y=NULL ){
    
    iY = which( Y>=1998 )  # no historical data prior to 1998
    if (length(iY)==0) return ("No data for specified years")
    Y = Y[iY]
 
    if (DS=="load") {
      # this safely replaces any updates
      dirlist = list.files(path=minilog.rawdata.location, full.names=T, recursive=T)
      dirlist = dirlist[ -grep("backup", dirlist) ]
      nfiles = length(dirlist)
      filelist = matrix( NA, ncol=3, nrow=nfiles) 
      for (f in 1:nfiles) {
        yr = minilogDate( fnMini=dirlist[f] ) 
        if (is.null(yr) ) next()
        if ( yr %in% Y ) filelist[f,] = c( f, dirlist[f], yr )
      }
      filelist = filelist[ which( !is.na( filelist[,1] ) ) , ]

      con <- dbConnect( dbDriver("SQLite"), mDB)
      dbSendQuery( con, " PRAGMA synchronous=OFF ;" ) # speeds up writes using buffers ... but no longer atomic
      for ( yr in Y ) {
        print(yr)
        minilog.delete( con, yr ) # remove any data matching this year
        fs = filelist[ which( as.numeric(filelist[,3])==yr ) , 2 ]
        if (length(fs)==0) next()
        for (f in 1:length(fs)) {
          j = load.minilog.rawdata( fs[f], unique.id=f)  # variable naming conventions in the past
          if (is.null(j)) next() 
          dbWriteTable(con, mMeta, j$metadata, overwrite=F, row.names=F, append=T)
          dbWriteTable(con, mBase, j$minilog.data, overwrite=F, row.names=F, append=T)
        }
      }
      dbDisconnect(con) 
      return ( mDB )
    }


    if(DS=="stats.redo") {
      set = snowcrab.db( DS="set.minilog") 
      con = dbConnect( dbDriver("SQLite"), mDB)
      dbSendQuery( con, " PRAGMA synchronous=OFF ;" ) # speeds up writes using buffers ... but no longer atomic
      tableExists = dbExistsTable( con, mStats )
      for ( yr in Y ) {
        print (yr )
        rid = dbGetQuery(con, paste( "SELECT unique_id FROM ", mMeta, " WHERE yr=", yr, ";", sep="" ) )
        if ( tableExists ) dbSendPreparedQuery( con, paste( "DELETE FROM ", mStats, " WHERE unique_id=:unique_id ;", sep=""),  rid )
        out = NULL
        for ( id in rid$unique_id ) {
          M = dbGetQuery(con, paste( "SELECT chron, depth, temperature FROM ", mBase, " WHERE unique_id='", id, "'", sep="" ) )
          iS = which(set$minilog_uid==id ) 
          if (length(iS) == 1) {
            res = bottom.contact( M , settimestamp=set$chron[iS], setdepth=set$Zx[iS] )
          } else {
            res = bottom.contact( M )
          }
          out = rbind( out, cbind( unique_id=id, res ) )
        }
        dbWriteTable(con, mStats, out, overwrite=F, row.names=F, append=T)
      }
      dbDisconnect(con) 
      return ( mDB )
    }

    if (DS %in% c("stats") ){
      con = dbConnect( dbDriver("SQLite"), mDB)
      yrs = paste(Y, collapse=",")
      qry = paste( "SELECT m.unique_id, t0, t1, dt, yr, timestamp, stationid, studyid, z, t, zsd, tsd, n, filename ",
              " FROM ", mMeta, " m LEFT OUTER JOIN ", mStats, " s", 
              " ON m.unique_id=s.unique_id",
              " WHERE yr IN (", yrs, ");"           
          , sep="" )
      
      res = dbGetQuery(con, qry ) 
      dbDisconnect(con) 
      res$t0 = string2chron( res$t0 )
      res$t1 = string2chron( res$t1 )
      res$timestamp = string2chron( res$timestamp )
      dt = res$dt
      res$dt = NA
      i = which(!is.na( dt ) )
      if (length(i) >0 ) res$dt[i] = times( dt[i] )
      return (res)
    }

    if (DS %in% c("metadata") ){  # dump all metadata
      con = dbConnect( dbDriver("SQLite"), mDB)
      yrs = paste(Y, collapse=",")
      M = dbGetQuery(con, paste( "SELECT * FROM ", mMeta, " WHERE yr IN (", yrs, ");", sep="" ) ) 
      if (nrow(M)!=0) M$timestamp = string2chron(M$timestamp)
      dbDisconnect(con) 
      return(M) 
    } 

    if (DS %in% c("basedata") ){
      con = dbConnect( dbDriver("SQLite"), mDB)
      yrs = paste(Y, collapse=",")
      M = dbGetQuery(con, paste( "SELECT m.yr, b.* FROM ", mMeta, " m LEFT JOIN ", mBase, " b ", 
              " ON m.unique_id=b.unique_id WHERE yr IN (", yrs, ");"
        , sep="" ) ) 
      dbDisconnect(con) 
      return(M) 
    } 
        # --------------------------------

    if (DS=="set.minilog.lookuptable" ) {
      
      # must fix:: individual record updates or yearly updates slow and not removing all relevent records
      # For now just refresh the whole table

      set = sqlite.read ( db.snow, "setInitial" )
      set$chron = string2chron ( set$chron )
      set$minilog_uid = ""
      nset = nrow( set )
      Y = sort(unique(set$yr))
      
      con = dbConnect(dbDriver("SQLite"), mDB )
      dbGetQuery( con, paste("ATTACH '", db.snow, "' AS snowcrab", sep="") ) 
      
      B = minilog.db( DS="metadata", Y )
      B$timestamp = string2chron ( B$timestamp)
      B = B[ which( is.finite( B$timestamp ) ) , ]
      B$startdate = convert.datecodes(B$timestamp, "tripcode")
      B$minilog_uid = B$unique_id 
  
      # match on trip and station number 
      # sequence is important here: first has priority ... do not modify the order !!
      # merge by trip, station .. exact matches  for the recent data series (2004 +)
      ina = grep("NA", B$startdate)
      if (length( ina > 0 ) )  B = B[ -ina ,]  # mostly 1998 data .. but these already have manually determined SA
       
      # primary matching -- at BIO, this is used since 2004 
    
      for (DT in c(5, 10, 20, 30)/60/24 )  {  
      
        noid = which( set$minilog_uid =="" )
        if (length(noid)==0 ) next()
        for (i in noid)  {        
          iii = which( 
              ( abs( as.numeric(B$timestamp - set$chron[i]) ) < DT) & 
              ( abs( as.numeric(B$t0 - set$chron[i] ) ) < DT )  & 
              ( B$stationid==set$station[i] ) )
          iii = iii[ which( is.finite( iii) ) ]
          iii = unique( iii )
          niii = length( iii )
          # accept only unique matches
          if ( niii == 1) set$minilog_uid[ i ] = B[ iii , "minilog_uid" ]
          if ( niii > 1 ) {
            ioo = grep( "bad", B$filename[iii], ignore.case=T ) 
            if ( length(ioo)>0 ) {
              iii = iii[-ioo] 
              if ( length(iii) == 1) {
                set$minilog_uid[ i ] = B[ iii , "minilog_uid" ]
              } else {
                print( "Multiple matches found: " )
                print( set$minilog_uid[ i ] )
                print( B[ iii, ] )
              }
            }
          }
        }
       
        noid = which( set$minilog_uid =="" )
        if (length(noid)==0 ) next()
        for (i in noid)  {        
          iii = which( 
              ( abs( as.numeric(B$timestamp - set$chron[i]) ) < DT) & 
              ( B$stationid==set$station[i] ) )
          iii = iii[ which( is.finite( iii) ) ]
          iii = unique( iii )
          niii = length( iii )
          # accept only unique matches
          if ( niii == 1) set$minilog_uid[ i ] = B[ iii , "minilog_uid" ]
          if ( niii > 1 ) {
            ioo = grep( "bad", B$filename[iii], ignore.case=T  ) 
            if ( length(ioo)>0 ) {
              iii = iii[-ioo] 
              if ( length(iii) == 1) {
                set$minilog_uid[ i ] = B[ iii , "minilog_uid" ]
              } else {
                print( "Multiple matches found: " )
                print( set$minilog_uid[ i ] )
                print( B[ iii, ] )
              }
            }
          }
        }
        
        noid = which( set$minilog_uid =="" )
        if (length(noid)==0 ) next()
        for (i in noid)  {        
          iii = which( 
              ( abs( as.numeric(B$t0 - set$chron[i] ) ) < DT )  & 
              ( B$stationid==set$station[i] ) )
          iii = iii[ which( is.finite( iii) ) ]
          iii = unique( iii )
          niii = length( iii )
          # accept only unique matches
          if ( niii == 1) set$minilog_uid[ i ] = B[ iii , "minilog_uid" ]
          if ( niii > 1 ) {
            ioo = grep( "bad", B$filename[iii], ignore.case=T  ) 
            if ( length(ioo)>0 ) {
              iii = iii[-ioo] 
              if ( length(iii) == 1) {
                set$minilog_uid[ i ] = B[ iii , "minilog_uid" ]
              } else {
                print( "Multiple matches found: " )
                print( set$minilog_uid[ i ] )
                print( B[ iii, ] )
              }
            }
          }
        }

        # time threshold to consider matching, in minutes .. start with fine to ensure precision at first
        # check all within +/- DT min of trawl set start (computer time) -- this is ~ an exact match
        noid = which( set$minilog_uid =="" )
        if (length(noid)==0 ) next()
        for (i in noid)  {       
          iii = which(  abs( as.numeric(B$timestamp - set$chron[i] ) ) < DT ) 
          iii = iii[ which( is.finite( iii) ) ]
          iii = unique( iii )
          niii = length( iii ) 
          if ( niii == 1 ) { # add only if the id is not used
            if (!  any(grepl( B[ iii , "minilog_uid" ], set$minilog_uid, ignore.case=T ) )) {   
              set$minilog_uid[ i ] = B[ iii , "minilog_uid" ]
          }} 
          if ( niii > 1 ) {
            ioo = grep( "bad", B$filename[iii], ignore.case=T  ) 
            if ( length(ioo)>0 ) {
              iii = iii[-ioo] 
              if ( length(iii) == 1) {
                set$minilog_uid[ i ] = B[ iii , "minilog_uid" ]
              } else {
                print( "Multiple matches found: " )
                print( set$minilog_uid[ i ] )
                print( B[ iii, ] )
              }
            }
          }
        }

        # check all within +/- DT min of trawl set start (GPS time)  -- this is ~ an exact match
        noid = which( set$minilog_uid =="" )
        if (length(noid)==0 ) next()
        for (i in noid)  {       
          iii = which( abs( as.numeric(B$t0 - set$chron[i] ) ) < DT ) 
          iii = iii[ which( is.finite( iii) ) ]
          iii = unique( iii )
          niii = length( iii ) 
          if ( niii == 1) { # add only if the id is not used
            if (!  any(grepl( B[ iii , "minilog_uid" ], set$minilog_uid, ignore.case=T )) ) {   
              set$minilog_uid[ i ] = B[ iii , "minilog_uid" ]
        }}}
      } # end DT 

      
      noid = which( set$minilog_uid =="" )
      if (length(noid)> 0 ) {
        for (i in noid)  {       
          iii = which( B$startdate==set$trip[i]  & B$stationid==set$station[i]  )
          iii = iii[ which( is.finite( iii) ) ]
          iii = unique( iii )
          niii = length( iii )
          # accept only unique matches
          if ( niii == 1) set$minilog_uid[ i ] = B[ iii , "minilog_uid" ]
          if ( niii > 1 ) {
            ioo = grep( "bad", B$filename[iii], ignore.case=T  ) 
            if ( length(ioo)>0 ) {
              iii = iii[-ioo] 
              if ( length(iii) == 1) {
                set$minilog_uid[ i ] = B[ iii , "minilog_uid" ]
              } else {
                print( "Multiple matches found: " )
                print( set$minilog_uid[ i ] )
                print( B[ iii, ] )
              }
            }
      }}}

      # the following are error checks 
      
      DT2 = 2  # in days -- longer time frame to catch small glitches in time stamps, etc.
      # check all within +/- DT2 min of trawl set start (computer time)
      # None found when used in 2010
      noid = which( set$minilog_uid =="" )
      if (length(noid)> 0 ) {
        for (i in noid)  {       
          iii = which( B$stationid==set$station[i] & abs( as.numeric(B$timestamp - set$chron[i] ) ) < DT2 )
          iii = iii[ which( is.finite( iii) ) ]
          iii = unique( iii )
          niii = length( iii ) 
          if ( niii == 1) { # add only if the id is not used
            if (!  any( grepl( B[ iii , "minilog_uid" ], set$minilog_uid, ignore.case=T )) ) {   
              # set$minilog_uid[ i ] = B[ iii , "minilog_uid" ]
                print( B[iii,] )
                print (set[,i])
                print ("-----------------------------------")
      }}}}
   
      # check all within +/- DT2 min of trawl set start (GPS time)
      # None found when used in 2010
      noid = which( set$minilog_uid =="" )
      if (length(noid)> 0 ) {
        for (i in noid)  {       
          iii = which( B$stationid==set$station[i]  & abs( as.numeric(B$t0 - set$chron[i] ) ) < DT2 )   
          iii = iii[ which( is.finite( iii) ) ]
          iii = unique( iii )
          niii = length( iii ) 
          if ( niii == 1) { # add only if the id is not used
            if (! any( grepl( B[ iii , "minilog_uid" ], set$minilog_uid, ignore.case=T ) ) ) {   
              # set$minilog_uid[ i ] = B[ iii , "minilog_uid" ]
              print( B[iii,] )
              print (set[i,] )
              print ("-----------------------------------")
      }}}}

      set =  set[, c("trip", "set", "minilog_uid" )]
      
      # clean data from a particular year before appending
      # Trying to do this on a record by record basis seems to cause loose ends .. redo who whole table for now
      # if (dbExistsTable(con, setMinilogLookup )) dbSendQuery( con, paste( "DROP TABLE ", setMinilogLookup, ";") )
      dbWriteTable(con, setMinilogLookup, set, overwrite=T, append=F, row.names=F)
      dbDisconnect(con)
    } 
     
	}
 

