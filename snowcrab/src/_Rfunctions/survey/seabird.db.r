	
	# mostly a copy over of the MINILOG functions with variable nemaes being replaced
	# some modifications to the 
 

  seabird.db = function( DS="", Y=NULL ){
    
    sb.dir = project.directory("snowcrab", "data", "seabird" )
    seabird.rawdata.location = file.path( sb.dir, "archive" )
    
    if (!is.null(Y)) {
        iY = which( Y>=2012 )  # no historical data prior to 2012
        if (length(iY)==0) return ("No data for specified years")
        Y = Y[iY]
    }

    if ( DS %in% c("basedata", "metadata", "load") ) {
  
      if (DS=="basedata" ){
        flist = list.files(path=sb.dir, pattern="basedata", full.names=T, recursive=FALSE)
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
          load( flist )
          out= rbind( out, basedata )
        }
        return( out )
      }

      if (DS=="metadata" ){
        flist = list.files(path=sb.dir, pattern="metadata", full.names=T, recursive=FALSE)
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
          load( flist )
          out= rbind( out, metadata )
        }
        return( out )
      }

      # default is to "load"
      #
      dirlist = list.files(path=seabird.rawdata.location, full.names=T, recursive=T)
      oo = grep("backup", dirlist)
      if (length(oo) > 0) dirlist = dirlist[ -oo ]
      nfiles = length(dirlist)
      filelist = matrix( NA, ncol=3, nrow=nfiles) 
      for (f in 1:nfiles) {
        yr = seabirdDate( fnSeaBird=dirlist[f] ) 
        if (is.null(yr) ) next()
        if ( yr %in% Y ) filelist[f,] = c( f, dirlist[f], yr )
      }
      filelist = filelist[ which( !is.na( filelist[,1] ) ) , ]

      set = snowcrab.db( DS="setInitial" ) 
      
      for ( yr in Y ) {
        print(yr)
        fn.meta = file.path( sb.dir, paste( "seabird", "metadata", yr, "rdata", sep="." ) )
        fn.raw = file.path( sb.dir, paste( "seabird", "basedata", yr, "rdata", sep="." ) )
        fs = filelist[ which( as.numeric(filelist[,3])==yr ) , 2 ]
        if (length(fs)==0) next()
        basedata = NULL
        metadata = NULL
        for (f in 1:length(fs)) {
          j = load.seabird.rawdata( fn=fs, f=f, set=set )  # variable naming conventions in the past
          if (is.null(j)) next()
          metadata = rbind( metadata, j$metadata)
          basedata = rbind( basedata, j$basedata)
        }
        
        save( metadata, file=fn.meta, compress=TRUE ) 
        save( basedata, file=fn.raw, compress=TRUE ) 
      }
      
      return ( sb.dir )
    }


    if (DS %in% c("stats", "stats.redo" ) ) {
      
      if (DS=="stats") {
       
        flist = list.files(path=sb.dir, pattern="stats", full.names=T, recursive=FALSE)
        if (!is.null(Y)) {
          mm = NULL
          for (yy in Y ) {
            ll = grep( yy, flist)
            if (length(ll)>0 ) mm = c( mm, ll) 
          }
          if (length(mm) > 0 ) flist= flist[mm]
        }

        sb.stat = NULL
        for ( i in flist ) {
          load( flist )
          sb.stat = rbind( sb.stat, sbStats )
        }
   
        sb.meta = seabird.db( DS="metadata", Y=Y )

        res = merge( sb.meta, sb.stat,  by="unique_id", all.x=TRUE, all.y=FALSE, sort=FALSE ) 
        res$timestamp = string2chron( res$timestamp )
        return (res)

      }
      
      # default action  is "stats.redo"
      #
      set = snowcrab.db( DS="set.seabird") 
      for ( yr in Y ) {
        print (yr )

        fn = file.path( sb.dir, paste( "seabird.stats", yr, "rdata", sep=".") )
        sbStats = NULL

        sbRAW = seabird.db( DS="basedata", Y=yr )
        rid = seabird.db( DS="metadata", Y=yr )

        for ( id in rid$unique_id ) {
          sso = unlist( strsplit( id, split=".", fixed=TRUE) )
          sso.trip = sso[2] 
          sso.set = as.numeric(sso[3] )
          sso.station = as.numeric(sso[4])

          Mi = which( sbRAW$unique_id == id )
          if (length( Mi) == 0 ) next()
          M = sbRAW[ Mi, ]
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
          if (length(iS) == 1) {
            res = bottom.contact( M , settimestamp=set$chron[iS], setdepth=set$Zx[iS] )
          } else {
            res = bottom.contact( M )
          }
          sbStats = rbind( sbStats, cbind( unique_id=id, res ) )
        }
        sbStats$unique_id =  as.character(sbStats$unique_id)
        sbdt = sbStats$dt
        sbStats$dt = NA
        i = which(!is.na( sbdt ) )
        if (length(i) >0 ) sbStats$dt[i] = times( sbdt[i] )

        save( sbStats, file=fn, compress=TRUE) 

      }
      
      return ( fn )
    }


    # ----------------------

    if ( DS %in% c("set.seabird.lookuptable", "set.seabird.lookuptable.redo" ) ) {
      
      fn = file.path( sb.dir, "set.seabird.lookuptable.rdata" )
      
      if ( DS=="set.seabird.lookuptable" ) {
        B = NULL
        if ( file.exists( fn) ) load (fn)

        return (B)
      }

      B = seabird.db( DS="metadata" )
      B$seabird_uid = B$unique_id
      B$station = B$stationid
     
      # duplicates are created sometime when datafiles are not reset completely and the same data is reloaded during the survey ... cleanup
      uuid = paste( B$trip, B$set, B$station, sep="." ) # seabird_uid cannot be used as it has a file number attached to make each one unique, even with duplicated data streams
      dups = which( duplicated( uuid) )
      if (length(dups > 0 ) ) B = B[ -dups, ]

      B =  B[, c("trip", "set", "station", "seabird_uid" )]
      save(B, file=fn, compress=TRUE )
      return(fn)

    } 
     
	}
 
  # --------------------


