	

  minilog.db = function( DS="", Y=NULL ){
      
    minilog.dir = project.directory("snowcrab", "data", "minilog" )
    minilog.rawdata.location = file.path( minilog.dir, "archive" )
    years.with.sets.combined = 2014 #the years where minilog not downloaded after each tow

    if (!is.null(Y)) {
      iY = which( Y>=1999 )  # no historical data prior to 1999
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
            if (length(ll)==0) return( NULL) # nothing to do
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
            if (length(ll)==0) return( NULL ) # nothing to do
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
      if (length(oo) > 0) {
        backups = dirlist[ oo ]
        dirlist = dirlist[-oo]
      }
    #  ii = NULL
    #  for(y in years.sets.combined) {
    #      io = grep(y,dirlist)
    #      ii = c(ii,io)
    #      }


      nfiles = length(dirlist)
      filelist = matrix( NA, ncol=3, nrow=nfiles) 
     
      for (f in 1:nfiles) {
        yr = minilogDate( fnMini=dirlist[f] ) 
        if (is.null(yr) ) next()
        if ( yr %in% Y ) filelist[f,] = c( f, dirlist[f], yr )
      }
      filelist = filelist[ which( !is.na( filelist[,1] ) ) , ]

      set = snowcrab.db( DS="setInitial" )  # set$chron is in ADT 
    
      for ( yr in Y ) {
        print(yr)
        fn.meta = file.path( minilog.dir, paste( "minilog", "metadata", yr, "rdata", sep="." ) )
        fn.raw = file.path( minilog.dir, paste( "minilog", "basedata", yr, "rdata", sep="." ) )
        fs = filelist[ which( as.numeric(filelist[,3])==yr ) , 2 ]

        if (length(fs)==0) next()

        basedata = NULL
        metadata = NULL

        for (f in 1:length(fs)) {
          if(!yr %in% years.with.sets.combined) j = load.minilog.rawdata( fn=fs[f], f=f, set=set)  # variable naming conventions in the past
          if(yr %in% years.with.sets.combined) j = load.minilog.rawdata.one.file.per.day( fn=fs[f], f=f, set=set)  # variable naming conventions in the past
          if (is.null(j)) next() 
          metadata = rbind( metadata, j$metadata)
          basedata = rbind( basedata, j$basedata)
        }
        
        # now do a last pass for the "backups" .... 
        # incomplete ....
        add.backup.minilogs=FALSE
        if (add.backup.minilogs) {
          fb = backups[ which( as.numeric(backups[,3])==yr ) , 2 ]
          for (f in 1:length(fb)) {
            j = load.minilog.rawdata.backups( fn=fb[f], f=f, set=set)  # variable naming conventions in the past
            if (is.null(j)) next() 
            metadata = rbind( metadata, j$metadata)
            basedata = rbind( basedata, j$basedata)
          }
        }

        save( metadata, file=fn.meta, compress=TRUE ) 
        save( basedata, file=fn.raw, compress=TRUE ) 

      }

      minilog.db( DS="set.minilog.lookuptable.redo" )
        
      return ( minilog.dir )
    }


    if (DS %in% c("stats", "stats.redo" ) ) {
      
      if (DS %in% c("stats") ){
        flist = list.files(path=minilog.dir, pattern="stats", full.names=T, recursive=FALSE)
        if (!is.null(Y)) {
          mm = NULL
          for (yy in Y ) {
            ll = grep( yy, flist)
            if (length(ll)==0) return(NULL) # nothing to do
            if (length(ll)>0 ) mm = c( mm, ll) 
          }
          if (length(mm) > 0 ) flist= flist[mm]
        }
        mini.stat = NULL
        for ( i in flist ) {
          load( i )
          mini.stat = rbind( mini.stat, miniStats )
        }
        mini.meta = minilog.db( DS="metadata", Y=Y )
        res = merge( mini.meta, mini.stat,  by="minilog_uid", all.x=TRUE, all.y=FALSE, sort=FALSE ) 
        return (res)
       }


      # "stats.redo" is the default action
      
      for ( yr in Y ) {
        print (yr )
        fn = file.path( minilog.dir, paste( "minilog.stats", yr, "rdata", sep=".") )
        miniStats = NULL
        miniRAW = minilog.db( DS="basedata", Y=yr )
        
        mta = minilog.db( DS="metadata", Y=yr )
        rid = minilog.db( DS="set.minilog.lookuptable" )
        rid = data.frame( minilog_uid=rid$minilog_uid, stringsAsFactors=FALSE )
        rid = merge( rid, mta, by="minilog_uid", all.x=TRUE, all.y=FALSE )
        rid = rid[ rid$yr== yr ,] 
         
        if (nrow(rid) == 0 ) next()
        
        for ( i in 1:nrow(rid)  ) {

          id = rid$minilog_uid[i]
          sso.trip = rid$trip[i] 
          sso.set = rid$set[i]
          sso.station = rid$station[i]

          Mi = which( miniRAW$minilog_uid == id )
          if (length( Mi) == 0 ) next()
          M = miniRAW[ Mi, ]
          
          M$timestamp = as.POSIXct( M$chron, tz="ADT" )
          settimestamp= as.POSIXct( rid$setChron[i] , tz="ADT" )
print(id)

  if(length(M$depth[!is.na(M$depth)])>15) {
          res = bottom.contact( id=id, x=M, settimestamp=settimestamp, setdepth=rid$setZx[i],
              tdif.min=3, tdif.max=9, eps.depth=3, sd.multiplier=3, depth.min=20, depth.range=30, depthproportion=0.5 )
} 
 if(length(M$depth[!is.na(M$depth)])==0) {
  res=list()
  #res$res = data.frame(yr=yr,timestamp = settimestamp, trip = sso.trip, set = sso.set, station = sso.station, studyid = rid[i,'studyid'], setZx = rid[i,'setZx'], setChron = rid[i,'setChron'],error = rid[i, 'error'], filename = rid[i,'filename'],
  # headerall = rid[i,'headerall'] , z = NA, t =  NA, zsd =NA, tsd =NA, n = NA,t0 =NA , t1=NA, dt =NA)
  res$res = data.frame(z = NA, t =  NA, zsd =NA, tsd =NA, n = NA,t0 =NA , t1=NA, dt =NA)
  #todo tie in the seabird data bottom contact to get secondary temperature data}
}
          if (FALSE) {
            # to visualize
            res = bottom.contact( id=id, x=M, settimestamp=settimestamp, setdepth=rid$setZx[i], 
              tdif.min=3, tdif.max=9, eps.depth=3, sd.multiplier=3, depth.min=20, depth.range=30, depthproportion=0.5, plot.data=TRUE )
          }

#          if (all (is.finite( res$smooth.method) ) ) {
            ## --- NOTE modal seems to work best ... but 
            # no single best method .. use the default which is the mean of all methods
            ##  likely due to greater precision and data density relative to minilog
#            res$res$t0 = res$smooth.method[1]
#            res$res$t0 = res$smooth.method[2]
#            res$res$dt = res$smooth.method[2] -  res$smooth.method[1]
#          }
         miniStats = rbind(miniStats, cbind( minilog_uid=id, res$res ) )
        }
        
        miniStats$minilog_uid =  as.character(miniStats$minilog_uid)
        minidt = miniStats$dt
        miniStats$dt = NA
        i = which(!is.na( minidt ) )
        if (length(i) >0 ) miniStats$dt[i] = times( minidt[i] )
  
        save( miniStats, file=fn, compress=TRUE )
      }

      return ( minilog.dir )
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
      
      # double check .. should not be necessary .. but in case
      uuid = paste( B$trip, B$set, sep="." ) 
      dups = which( duplicated( uuid) )

      if (length(dups > 0 ) ) {
        toremove =NULL
        for (i in dups) {
          di = which( uuid == uuid[i] )
          tdiff = B$setChron[di] - B$timestamp[di]
          oo = which.min( abs( tdiff) ) 
          toremove = c(toremove, di[-oo] )
          print("----")
          print( "Matching based upon closest time stamps")
          print(B[di, ])
          print( "Choosing: ")
          print(B[di[oo], ])
          print("")
          toremove = c(toremove, di[-oo] )
        }
        B = B[ -toremove, ]
      }
      B = B[, c("trip", "set", "minilog_uid" )]
      save(B, file=fn, compress=TRUE )
      return(fn)
    } 
	}
 

