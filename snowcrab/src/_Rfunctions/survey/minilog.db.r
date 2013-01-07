	

  minilog.db = function( DS="", Y=NULL ){
      
    minilog.dir = project.directory("snowcrab", "data", "minilog" )
    minilog.rawdata.location = file.path( minilog.dir, "archive" )
   
    if (!is.null(Y)) {
      iY = which( Y>=1998 )  # no historical data prior to 1998
      if (length(iY)==0) return ("No data for specified years")
      Y = Y[iY]
    }


    if ( DS %in% c("basedata", "metadata", "load") ) {
       
      if (DS=="basedata" ){
        flist = list.files(path=minilog.dir, pattern="basedata", full.names=T, recursive=FALSE)
        if (!is.null(Y)) {
          mm = NULL
          for (yy in Y ) {
            ll = grep( yy, flist)
            if (length(ll)>0 ) mm = c( mm, ll) 
          }
          if (length(mm) > 0 ) flist= flist[mm]
        }
        out = NULL
        for ( i in flist ) {
          load( i )
          out= rbind( out, basedata )
        }
        return( out )
      }

      if (DS=="metadata" ){
        flist = list.files(path=minilog.dir, pattern="metadata", full.names=T, recursive=FALSE)
        if (!is.null(Y)) {
          mm = NULL
          for (yy in Y ) {
            ll = grep( yy, flist)
            if (length(ll)>0 ) mm = c( mm, ll) 
          }
          if (length(mm) > 0 ) flist= flist[mm]
        }
        out = NULL
        for ( i in flist ) {
          load( i )
          out= rbind( out, metadata )
        }
        return( out )
      }

      # default is to "load"
      #

      dirlist = list.files(path=minilog.rawdata.location, full.names=T, recursive=T)
      oo = grep("backup", dirlist)
      if (length(oo) > 0) dirlist = dirlist[ -oo ]
      nfiles = length(dirlist)
      filelist = matrix( NA, ncol=3, nrow=nfiles) 
      for (f in 1:nfiles) {
        yr = minilogDate( fnMini=dirlist[f] ) 
        if (is.null(yr) ) next()
        if ( yr %in% Y ) filelist[f,] = c( f, dirlist[f], yr )
      }
      filelist = filelist[ which( !is.na( filelist[,1] ) ) , ]

      set = snowcrab.db( DS="setInitial" ) 
    
      for ( yr in Y ) {
        print(yr)
        fn.meta = file.path( minilog.dir, paste( "minilog", "metadata", yr, "rdata", sep="." ) )
        fn.raw = file.path( minilog.dir, paste( "minilog", "basedata", yr, "rdata", sep="." ) )
        fs = filelist[ which( as.numeric(filelist[,3])==yr ) , 2 ]

        if (length(fs)==0) next()

        basedata = NULL
        metadata = NULL

        for (f in 1:length(fs)) {
          j = load.minilog.rawdata( fn=fs[f], f=f, set=set)  # variable naming conventions in the past
          if (is.null(j)) next() 
          metadata = rbind( metadata, j$metadata)
          basedata = rbind( basedata, j$basedata)
        }
        
        save( metadata, file=fn.meta, compress=TRUE ) 
        save( basedata, file=fn.raw, compress=TRUE ) 

      }
      return ( minilog.dir )
    }


    if (DS %in% c("stats", "stats.redo" ) ) {
      
      if (DS %in% c("stats") ){
        flist = list.files(path=minilog.dir, pattern="stats", full.names=T, recursive=FALSE)
        if (!is.null(Y)) {
          mm = NULL
          for (yy in Y ) {
            ll = grep( yy, flist)
            if (length(ll)>0 ) mm = c( mm, ll) 
          }
          if (length(mm) > 0 ) flist= flist[mm]
        }
        mini.stat = NULL
        for ( i in flist ) {
          load( flist )
          mini.stat = rbind( mini.stat, miniStats )
        }
        mini.meta = minilog.db( DS="metadata", Y=Y )
        res = merge( mini.meta, mini.stat,  by="unique_id", all.x=TRUE, all.y=FALSE, sort=FALSE ) 
        return (res)
       }


      # "stats.redo" is the default action
      #
      set = snowcrab.db( DS="set.minilog") 
      
      for ( yr in Y ) {
        print (yr )
        fn = file.path( minilog.dir, paste( "minilog.stats", yr, "rdata", sep=".") )
        miniStats = NULL
        miniRAW = minilog.db( DS="basedata", Y=yr )
        rid = minilog.db( DS="metadata", Y=yr )
        for ( i in 1:nrow(rid)  ) {

          id = rid$unique_id[i]
          sso.trip = rid$trip[i] 
          sso.set = rid$set[i]
          sso.station = rid$stationid[i]

          Mi = which( miniRAW$unique_id == id )
          if (length( Mi) == 0 ) next()
          M = miniRAW[ Mi, ]
          iS = which(set$seabird_uid==id ) 
          if (length(iS) != 1) {
            # probably due to a duplicated seabird entry
            iS = which( set$trip==sso.trip & set$set==sso.set & set$station==sso.station )
            if (length( iS) != 1 ) {
              iS = which( set$trip==sso.trip & set$set==sso.set )
                if (length( iS) != 1 ) {
                  iS = which( set$trip==sso.trip & set$station==sso.station )
                }
            }
          }

          iS = which(set$minilog_uid==id ) 
          if (length(iS) == 1) {
            res = bottom.contact( M , settimestamp=set$chron[iS], setdepth=set$Zx[iS] )
          } else {
            res = bottom.contact( M )
          }
          miniStats = rbind(miniStats, cbind( unique_id=id, res ) )
        }
        
        miniStats$unique_id =  as.character(miniStats$unique_id)
        minidt = miniStats$dt
        miniStats$dt = NA
        i = which(!is.na( minidt ) )
        if (length(i) >0 ) miniStats$dt[i] = times( minidt[i] )
      
      }
      return ( fn )
    }

    # --------------------------------

    if (DS %in% c("set.minilog.lookuptable", "set.minilog.lookuptable.redo") ) {
  
      fn = file.path( minilog.dir, "set.minilog.lookuptable.rdata" )
     
      if (DS=="set.minilog.lookuptable" ) {
        B = NULL
        if ( file.exists( fn) ) load (fn)
        return (B)
      }
     
      B = minilog.db( DS="metadata" )
      B$minilog_uid = B$unique_id 
      B$station = B$stationid

      B = B[, c("trip", "set", "station", "minilog_uid" )]
      
      # double check .. should not be necessary .. but in case
      uuid = paste( B$trip, B$set, B$station, sep="." ) 
      dups = which( duplicated( uuid) )
      if (length(dups > 0 ) ) B = B[ -dups, ]

      save(B, file=fn, compress=TRUE )
      return(fn)

    } 
     
	}
 

