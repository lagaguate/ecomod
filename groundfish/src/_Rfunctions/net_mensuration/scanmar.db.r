

scanmar.db = function( DS, p, nm=NULL, id=NULL, YRS=NULL, bottom.contact.debug.id=NULL){
 
  perley.years0 = c( 1990:1992 )
  perley.years1 = c( 2004:2009 )
  datalog.year0 = 2009    # note this overlaps with the last year in perley's database 


  if (!file.exists( p$scanmar.dir )) {
   mk.Dir = readline("Directory not found, shall we create it? Yes/No (Default No) : ")
    if ( mk.Dir =="Yes" ) {
      dir.create( p$scanmar.dir, recursive=TRUE) 
      print( paste("The directory -- ",  p$scanmar.dir, " -- has been created") )
    } else {
      warning( paste("Directory: ", p$scanmar.dir, " was not found." )) 
    }
  }


  if(DS %in% c("perley", "perley.datadump" )) {
    
    # fn1= old data, fn2= new data and fn3= merged data (old and new)
    fn1= file.path(p$scanmar.dir,"scanmar_rawdata_perley0.rdata")
    fn2= file.path(p$scanmar.dir,"scanmar_rawdata_perley1.rdata")
    fn3= file.path(p$scanmar.dir,"scanmar.perley.merged.rdata")
   
    if(DS=="perley"){
      nm = NULL
      if (file.exists(fn3)) load(fn3)
      return(nm)
    }

    if(DS=="perley.redo"){
 
      # Package RODBC is a platform for interfacing R to database systems
      # Package includes the obdc* commands (access) and sql* functions (read, save, copy, and manipulate data between data frames)
      if (file.exists( fn1) ) {
        load(fn1)
      } else {
        require(RODBC)
        connect=odbcConnect( oracle.perley.db, uid=oracle.perley.user, pwd=oracle.perley.password, believeNRows=F)
        scanmar = sqlQuery(connect, "select * from   groundfish.perleyp_SCANMAR", as.is=T) 
        odbcClose(connect)
        save(scanmar, file=fn1, compress=T)
      }
    
      if (file.exists( fn2) ) {
        load(fn2)
      } else {
        require(RODBC)
        connect=odbcConnect( oracle.perley.db, uid=oracle.perley.user, pwd=oracle.perley.password, believeNRows=F)
        scanmarnew = sqlQuery(connect, "select * from   groundfish.perleyp_NEWSCANMAR", as.is=T) 
        odbcClose(connect)
        save(scanmarnew, file=fn2, compress=T)
      }

      nm = scanmar
      rm(scanmar)
      gc()

      names(nm)=tolower(names(nm))      #ac
      names(scanmarnew)=tolower(names(scanmarnew))      
      
      # nm is the dataset which combines the old and new data (merged)
      # some variables were missing from scanmar to faciliate the merge of scanmarnew
      nm$fspd=NA
      nm$cspd=NA        
      nm$latitude=NA
      nm$longitude=NA
      nm$depth=NA
      nm$empty=NA
      nm$time=NA

      # Correcting for data which contains NA in the time slot by identifying and deleting it
      strangedata = which(is.na(nm$logtime))
      if(length(strangedata)>0) nm=nm[-strangedata,]

      # fix some time values that have lost the zeros due to numeric conversion
      nm$logtime=gsub(":", "", nm$logtime)      
      j=nchar(nm$logtime)
      tooshort=which(j==5); if (length(tooshort)>0) nm$logtime[tooshort]=paste("0",nm$logtime[tooshort],sep="")
      tooshort=which(j==4); if (length(tooshort)>0) nm$logtime[tooshort]=paste("00",nm$logtime[tooshort],sep="")
      tooshort=which(j==3); if (length(tooshort)>0) nm$logtime[tooshort]=paste("000",nm$logtime[tooshort],sep="")
      tooshort=which(j==2); if (length(tooshort)>0) nm$logtime[tooshort]=paste("0000",nm$logtime[tooshort],sep="")
      tooshort=which(j==1); if (length(tooshort)>0) nm$logtime[tooshort]=paste("00000",nm$logtime[tooshort],sep="")
            
      nm$hours=substring(nm$logtime,1,2)
      nm$min=substring(nm$logtime,3,4)
      nm$sec=substring(nm$logtime,5,6)
      nm$time = paste(nm$hours, nm$min, nm$sec, sep=":")
      
      # creating a matrix (nm2) with nm and scanmarnew
      nm2=matrix(NA,ncol=ncol(nm),nrow=nrow(scanmarnew))
      nm2= as.data.frame(nm2)
      names(nm2)=names(nm)
      
      # making the columns names of nm2 equal to those of scanmarnew
      o =names(scanmarnew)
      for(n in o){
        nm2[,n]=scanmarnew[,n]
      }
      
      # Combining rows of nm and nm2 to create the data frame nm
      nm=rbind(nm,nm2)      
      rm(scanmarnew, nm2)
      gc()
     
      #This step is creating variables by pasting existing together
      #It is also changing character values to numeric
      nm$uniqueid=paste(nm$mission,nm$setno,sep="_")
      nm$ltspeed=as.numeric(nm$ltspeed)
      nm$ctspeed=as.numeric(nm$ctspeed)
      nm$doorspread=as.numeric(nm$doorspread)
      nm$wingspread=as.numeric(nm$wingspread)
      nm$clearance=as.numeric(nm$clearance)
      nm$opening=as.numeric(nm$opening)
      nm$depth=as.numeric(nm$depth)
      nm$latitude=as.numeric(nm$latitude)
      nm$longitude=as.numeric(nm$longitude)
      nm$year.mission= as.numeric(substring(nm$mission,4,7))
      nm$fspd=as.numeric(nm$fspd)
      nm$cspd= as.numeric(nm$cspd)
      
      # merge groundfish  timestamps and ensure that net mensuration timestamps are correct
      nm$id=paste(nm$mission, nm$setno, sep=".")
      ii = which( nm$longitude > 0 )
      if (length(ii) > 0 ) nm$longitude[ii] = - nm$longitude[ii] 
      
      # load groundfish inf table which has timestamps of start/stop times and locations
      gsinf = groundfish.db( DS="gsinf" )
      gsinfvars=c("id", "sdate", "settype" )
      
      # merge 
      nm = merge( nm, gsinf[,gsinfvars], by="id", all.x=TRUE, all.y=FALSE)
      
      nm$day = lubridate::day( nm$sdate )
      nm$mon = lubridate::month( nm$sdate )
      nm$yr = lubridate::year( nm$sdate )
      nm$date = paste(nm$yr, nm$mon, nm$day, sep="-")
    
      i = which(!is.finite(nm$day))
      if (length(i)>0) nm = nm[ -i, ]

      i = which( is.na( nm$time))
      if (length(i)>0) nm = nm[ -i, ]
 
      nm$tstamp= paste( nm$date, nm$time )
      
      tzone = "America/Halifax"  ## need to verify if this is correct
  
      #lubridate function 
      nm$timestamp = ymd_hms(nm$tstamp)
      tz( nm$timestamp )=tzone

      keep=c("id", "vesel", "ltspeed", "ctspeed", "wingspread", "doorspread", "clearance",
             "opening", "fspd", "cspd", "latitude", "longitude", "depth", "settype", "timestamp", "yr"
             )
      nm=nm[,keep]

      # fix sets that cross midnight and list
      # some sets cross midnight and require days to be adjusted
      nm$timestamp = timestamp.fix (nm$timestamp, threshold.hrs=2 )
       
      nm$netmensurationfilename = "Perley Oracle instance"
      w = which(!is.finite(nm$cspd))
      nm$ctspeed[w]=nm$cspd[w]
      v = which(!is.finite(nm$fspd))
      nm$ltspeed[v]=nm$fspd[v]
      nm$gyro=NA  
      v.to.drop = c("vesel", "empty", "logtime", "cspd", "fspd", "settype", "dist", "edate" )
      for ( v in v.to.drop) nm[,v] = NULL
      
      save(nm, file=fn3,compress=TRUE)
    }
  }

  # -------------------------------
  
  if(DS %in% c("basedata", "basedata.redo"))  {
   
    if (is.null (YRS) ) YRS = p$netmensuration.years 
      
    dir.create( file.path( p$scanmar.dir, "basedata"), recursive=TRUE, showWarnings=FALSE ) 
    
    if(DS == "basedata"){
      out = NULL
      for ( YR in YRS ) {
        basedata=NULL
        fn = file.path( p$scanmar.dir,  "basedata",  paste( "scanmar", "basedata", YR, "rdata", sep="." ))
        if ( file.exists(fn)) {
          load(fn)
          out = rbind( out, basedata )
        }
      }
      return(out)
    }

    varnames = c( "id", "ltspeed", "ctspeed", "wingspread", "doorspread", "clearance", "opening", 
                  "latitude", "longitude", "depth", "gyro", "timestamp", "netmensurationfilename")

    rawdata.dir = file.path( p$scanmar.dir, "datalogs" )
    
    for ( YR in YRS ) {
      basedata = NULL
      fn = file.path( p$scanmar.dir, "basedata", paste( "scanmar", "basedata", YR, "rdata", sep="." ))
      
      if (YR %in% c( perley.years0, perley.years1)  ) {
        nm = scanmar.db( DS="perley", p=p )
        nm$yr =  lubridate::year(nm$timestamp)
        oo = which( nm$yr == YR )
        if (length( oo) > 0 ) {
          basedata = nm[oo,]
          basedata = basedata[ ,varnames ]
        }
        rm(nm); gc()  
      }

      # if YR == 2009 .. then the next step will add the log data as well
      if (YR >= datalog.year0 ) {   
        filelist = list.files(path=file.path( rawdata.dir, YR ), pattern="\\.log$", 
                              full.names=T, recursive=TRUE, ignore.case=TRUE)
        unneeded = grep ("copy", filelist, ignore.case=TRUE)
        if (length(unneeded>0)) filelist = filelist[-unneeded]
        unneeded = grep ("all.log", filelist, ignore.case=TRUE)
        if (length(unneeded>0)) filelist = filelist[-unneeded]
        
        if (length( filelist) > 0 ) {
          print( "Reading in scanmar log files:" )
          for ( fl in filelist ) {
            print(fl)
            j = load.scanmar.rawdata( fl, yr=YR )  # variable naming conventions in the past  .. always UTC
            if (is.null(j)) next()
            j$ctspeed=NA  # missing in modern data so add to permit merging with historical data
            j$id = NA # this will be filled in later once the position/time is matched to gsinf
            j = j[ , varnames ]
            basedata = rbind( basedata, j)
          }
        }
      }
      if (!is.null( basedata) ) {
        save(basedata, file=fn, compress= TRUE)
        print(fn)
      }
    } 
    return( YRS )
  }


  # -------------------------------------


  if (DS %in% c("basedata.lookuptable", "basedata.lookuptable.redo"))  {
    ## RAtionale: data from 2009 to 2014+ are missing mission/set inforamtion
    ## we need to match sets ("id") with scanmar data ("nm_id") using time and gpstrack / location information
    
    dir.create( file.path( p$scanmar.dir, "basedata.lookuptable"), recursive=TRUE, showWarnings=FALSE ) 
  
    if (is.null (YRS) ) YRS = p$netmensuration.years 
    
    if(DS == "basedata.lookuptable"){
      out = NULL
      for ( YR in YRS ) {
        meta = NULL
        fn = file.path( p$scanmar.dir, "basedata.lookuptable", paste( "scanmar", "basedata.lookuptable", YR, "rdata", sep= "."))
        if ( file.exists(fn)) {
          load(fn)
          out = rbind( out, meta )
        }
      }
      return(out)
    }
      
    gf = groundfish.db(DS="gsinf")
    
    for ( YR in YRS ) {
      
      fn = file.path( p$scanmar.dir, "basedata.lookuptable", paste( "scanmar", "basedata.lookuptable", YR, "rdata", sep= "."))
      
      # Incorporation of newer data, combining timestamp
      nm=scanmar.db( DS="basedata", p=p, YRS=YR ) 
      if (is.null( nm)) next()

      nm$lon=nm$longitude
      nm$lat=nm$latitude
      nm$longitude =NULL
      nm$latitude =NULL
      
      if (YR %in% c( perley.years0, perley.years1)  ) {
        nm$id = nm$nm_id = nm$id  
      }
      if (YR == datalog.year0 ) {   
        nm$nm_id = nm$id
        oo = which( nm$netmensurationfilename != "Perley Oracle instance") 
        if (length(oo) > 0)  {
          nm$id[oo] = nm$nm_id[oo] = nm$netmensurationfilename[ oo ]
        }
      }
      if (YR > datalog.year0 ) {   
        nm$nm_id = nm$netmensurationfilename
      }

      meta = data.frame( nm_id=unique( nm$nm_id), stringsAsFactors=FALSE )
      meta$timestamp = NA
      meta$id=NA
      meta$min.distance = NA
      meta$time.difference = NA
      meta$exact.match = FALSE

      # Perley series has id's
      pp = grep( "*.log$", meta$nm_id, invert=TRUE, ignore.case=TRUE )
      if (length( pp)>0) meta$id[pp] = meta$nm_id[pp]

      unmatched = which( is.na( meta$id))
      if (length(unmatched)>0) {
        ## first pass ... match based upon time and space contraints
        for(i in unmatched){
          k = meta$nm_id[i]
          j = which(nm$nm_id == k)
          if(length(j)>0) {
            ppc=nm[j,]
            cc = which( is.finite( ppc$lon + ppc$lat)  )
            if (length (cc) == 0 ) next()
            m = which.min(ppc$timestamp)
            meta$timestamp[i] = as.character(ppc$timestamp[m])
            dif = as.duration(ymd_hms(meta$timestamp[i]) - gf$timestamp)
            u = which(abs(dif)< dhours  (1) )  # time constraint here .. 1 hr
            
            if(length(u) == 1) {
              # most likely correct match but test for  distance to ensure match
              gfs=gf[u,]
              distance.test = geodist(ppc[,c("lon","lat")], gfs[,c("lon","lat")], method="great.circle")
              if( distance.test < 1 ){  # if less than 1 km distance
                meta$id[i]=gfs$id  # exact match with very high confidence
                meta$min.distance[i] = distance.test
                meta$time.difference[i] = dif[ u ]
                meta$exact.match = TRUE
              } 
            }
         
            if(length(u) > 1) {
              gfs=gf[u,]
              gfs$min.distance.test=NA
              for(v in 1:nrow (gfs)){
                distance.test = geodist(ppc[,c("lon","lat")], gfs[v,c("lon","lat")], method="great.circle")
                gfs$min.distance.test[v] = min(distance.test, na.rm=TRUE)
              }
              w = which.min(gfs$min.distance.test)
              if(gfs$min.distance.test[w]< 1 ){  # if less than 1 km distance
                meta$id[i]=gfs$id[w]  # exact match with very high confidence
                meta$min.distance[i] = gfs$min.distance.test[w]
                meta$time.difference[i] = dif[u[w]]
                meta$exact.match = TRUE
              } 
            }
          }
        }

        # Check for duplicates as some are data errors .. needed to be checked manually and raw data files altered
        # others are due to bad tows being redone ... so invoke a distance based rule as the correct one in gsinf 
        # (good tows only are recorded)
        dupids = unique( meta$id[ which( duplicated( meta$id, incomparables=NA) ) ] )
        if (length(dupids)>0) {
          for ( dups in dupids ) {
            uu = which(meta$id %in% dups)
            if ( any( meta$exact.match[uu] ) ) {
              pp = which(  meta$exact.match[uu] ) 
              qq = which( !meta$exact.match[uu] ) 
              if ( length(pp) == 1 ) meta$id[uu[qq]] = NA
            } else {
              vv = which( meta$min.distance[uu] < 1 )  # ensure less than 1 km away
              ww = which.min( meta$min.distance[uu[vv]] )
              good = uu[vv[ww] ]
              if ( length( good)==1 ) {
                notsogood = setdiff( uu, good )    
                meta$id[notsogood] = NA 
              } else {
                meta$id[uu] = NA  # set all to NA unless a single solution is found
              }
            }
          }
        }  
        
        ## second pass: relax time constraint in case there were errors of time stamps ...
        unmatched = which( is.na( meta$id) )
        for(i in unmatched){
          k = meta$nm_id[i]
          j = which(nm$nm_id == k)
          if(length(j)>0) {
            ppc=nm[j,]
            cc = which( is.finite( ppc$lon + ppc$lat)  )
            if (length (cc) == 0 ) next()
            m = which.min(ppc$timestamp)
            meta$timestamp[i] = as.character(ppc$timestamp[m])
            dif = as.duration(ymd_hms(meta$timestamp[i]) - gf$timestamp)
            u = which(abs(dif)< dhours  (5) ) # <<<<------------ in case of timezone confusion between Atlantic vs UTC (3-4 hr diff) ..
            if(length(u) >= 1) {
              gfs=gf[u,]
              gfs$min.distance.test=NA
              for(v in 1:nrow (gfs)){
                distance.test = geodist(ppc[,c("lon","lat")], gfs[v,c("lon","lat")], method="great.circle")
                gfs$min.distance.test[v] = min(distance.test, na.rm=TRUE)
              }
              w = which.min(gfs$min.distance.test)
              if(gfs$min.distance.test[w]< 1 ){  # if less than 1 km distance
                meta$id[i]=gfs$id[w]  # exact match with very high confidence
                meta$min.distance[i] = gfs$min.distance.test[w]
                meta$time.difference[i] = dif[u[w]]
              } 
            }
          }
        }
      }
      save(meta, file= fn, compress= TRUE)
      print(fn)
    }  # end loop for YRS

    return( YRS )
  }

  
  # -------------------------------------
 

  if ( DS %in% c("sanity.checks", "sanity.checks.redo") ) {
   # Step to filter data  
    
   if (is.null (YRS) ) YRS = p$netmensuration.years 
   dir.create( file.path( p$scanmar.dir, "sanity.checked"), recursive=TRUE, showWarnings=FALSE )

   if(DS=="sanity.checks") {
      out = NULL
      for ( YR in YRS ) {
        nm = NULL
        fn = file.path( p$scanmar.dir, "sanity.checked", paste("scanmar.sanity.checked", YR, "rdata", sep=".") )
        if ( file.exists(fn)) {
          load(fn)
          out = rbind( out, nm )
        }
      }
      return(out)
   }
  

   for ( YR in YRS ) {
      meta =  scanmar.db( DS="basedata.lookuptable", p=p, YRS=YR )
      if (is.null(meta)) next()
 
      nm = scanmar.db( DS="basedata", p=p, YRS=YR ) 
      if (is.null( nm )) next()
 
      meta = meta[ which(!is.na( meta$nm_id)),]
      meta$timestamp =NULL # redundant
     
      if (YR %in% setdiff(c( perley.years0, perley.years1), datalog.year0)  ) {
        nm$nm_id = nm$id
        nm = merge(nm, meta, by="nm_id", all.x=TRUE, all.y=FALSE, , suffixes=c("", ".meta"))
        oo = which (is.na( nm$id))
        if (length(oo)>0) nm$id[oo] = nm$id.meta[oo]
      }
      
      if (YR == datalog.year0 ) {   
        nm$nm_id = nm$id
        oo = which( nm$netmensurationfilename != "Perley Oracle instance") 
        if (length(oo) > 0) nm$nm_id[oo] = nm$netmensurationfilename[ oo ]
        nm = merge(nm, meta, by="nm_id", all.x=TRUE, all.y=FALSE, suffixes=c("", ".meta") )
        pp =  which (is.na( nm$id))
        if (length(pp)>0) nm$id[pp] = nm$id.meta[pp]
      }
      
      if (YR > datalog.year0 ) {   
        # id not yet generated .. use filename
        nm$nm_id = nm$netmensurationfilename
        nm = merge(nm, meta, by="nm_id", all.x=TRUE, all.y=FALSE, suffixes=c("", ".meta") )
        nm$id = nm$id.meta
      }

      todrop = grep( ".meta$", names(nm))
      if (length(todrop)>0) nm = nm[ ,-todrop]

      nm$date = substring(as.character(nm$timestamp), 1,10)
      gooddata = which( !is.na( nm$id))
      
      
      ids = strsplit( nm$id[gooddata], "[.]" )
      
      mission.trip = unlist( lapply( ids, function(x){x[[1]]} ) )
      setno = unlist( lapply( ids, function(x){x[[2]]} ) )
      
      nm$set = NA
      nm$set[gooddata] = as.numeric( setno )
      
      nm$trip = NA
      nm$trip[gooddata] = substring( mission.trip, 8,10 )
      nm$trip = as.numeric(nm$trip)
      nm$year= lubridate::year(nm$timestamp)  
    
      # empty variable is not needed (crossed channel with doorspread), also values present look erroneous in NED2005001 1
      i = which( nm$id=="NED2005001.1" )
      if (length(i) >0) {
        nm$doorspread[i] = NA
        nm$wingspread[i] = NA
        nm$clearance[i] = NA
        nm$opening[i] = NA
        nm$ltspeed[i] = NA
        nm$ctspeed[i] = NA
      }

     # coarse level gating   
      
     # ID sets where American trawls were used for comparative surveys
     nm$net = "WesternIIA"
     oo = filter.nets("identify.trawls.with.US.nets", nm) 
     if (length( oo) >0 ) {
       nm$net[oo] = "American"
       ## additional gating goes here ... currently using the same rules .. 
       nm$doorspread[oo] = filter.nets("doorspread.range", nm$doorspread[oo] )
       nm$wingspread[oo]  = filter.nets("wingspread.range", nm$wingspread[oo] )
       nm$clearance[oo]  = filter.nets("clearance.range", nm$clearance[oo] )
       nm$opening[oo]  = filter.nets("opening.range", nm$opening[oo] )
       nm$depth[oo]  = filter.nets("depth.range", nm$depth[oo] )
     }

     pp = which( nm$net == "WesternIIA" )
     if (length(pp) >0 ) {
       nm$doorspread[pp] = filter.nets("doorspread.range", nm$doorspread[pp] )
       nm$wingspread[pp]  = filter.nets("wingspread.range", nm$wingspread[pp] )
       nm$clearance[pp]  = filter.nets("clearance.range", nm$clearance[pp] )
       nm$opening[pp]  = filter.nets("opening.range", nm$opening[pp] )
       nm$depth[pp]  = filter.nets("depth.range", nm$depth[pp] )
#   nm$door.and.wing.reliable[pp]  = filter.nets( "door.wing", nm[pp]  )    # flag to ID data that are bivariately stable .. errors still likely present
     }

     fn = file.path( p$scanmar.dir, "sanity.checked", paste("scanmar.sanity.checked", YR, "rdata", sep=".") )
     save( nm, file=fn, compress=TRUE)
     print(fn)
   }
   return (YRS )
  }


  # -------------------


  if (DS %in% c("bottom.contact", "bottom.contact.redo", "bottom.contact.id" )) {
    
    if (is.null (YRS) ) YRS = p$netmensuration.years 

    scanmar.bc.dir =  file.path(p$scanmar.dir, "bottom.contact" )
    dir.create( scanmar.bc.dir, recursive=TRUE, showWarnings=FALSE ) 
    dir.create (file.path( scanmar.bc.dir, "results"), recursive=TRUE, showWarnings=FALSE )
    dir.create (file.path( scanmar.bc.dir, "figures"), recursive=TRUE, showWarnings=FALSE )

    if(DS=="bottom.contact"){
      out = NULL
      for ( YR in YRS ) {
        gsinf=NULL
        fn= file.path( scanmar.bc.dir, paste( "gsinf.bottom.contact", YR, "rdata", sep=".")  )
        if (file.exists(fn)) load(fn) 
        out = rbind( out, gsinf )
      }
      gsinf0 = groundfish.db( DS="gsinf" )
      if (!is.null(out)) gsinf0 = merge( gsinf0, out, by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )
      gg = which( lubridate::year(gsinf0$sdate) %in% YRS )
      gs = NULL
      if (length(gg)>0) gs = gsinf0[ gg, ]
      return(gs)
    }

    if(DS=="bottom.contact.id"){
      fn.bc = file.path( scanmar.bc.dir, "results", paste( "bc", id, "rdata", sep=".") )
      bc = NULL
      if (file.exists(fn.bc)) load(fn.bc)
      return(bc)
    }
    
    fn.current = file.path( scanmar.bc.dir, "bottom.contact.tmp.current" )
    fn.badlist = file.path( scanmar.bc.dir, "bottom.contact.badlist" )
    fn.gsinf = file.path( scanmar.bc.dir, "bottom.contact.tmp.gsinf" )

    if ( file.exists (fn.current) ) file.remove( fn.current )
    if ( file.exists (fn.badlist) ) file.remove( fn.badlist )
    if ( file.exists (fn.gsinf) ) file.remove( fn.gsinf )
      
    badlist = skip = cur = NULL

    gsinf0 = groundfish.db( DS="gsinf" )
 
    gsinf0$year = lubridate::year( gsinf0$sdate )

    gsinf0.names = names( gsinf0 )

    for ( YR in YRS ) {
      yy = which( gsinf0$year == YR )
      if (length(yy)==0 ) next()
      gsinf = gsinf0[ yy, ]  
      gsinf$bottom_duration = NA
      gsinf$bc0.datetime = as.POSIXct(NA)
      gsinf$bc1.datetime = as.POSIXct(NA)
      gsinf$bc0.sd = NA
      gsinf$bc1.sd = NA
      gsinf$bc0.n = NA
      gsinf$bc1.n = NA
     
      nm = scanmar.db( DS="sanity.checks", p=p, YRS=YR )
      if (is.null( nm)) next()
      nm = nm[which(is.finite(nm$depth)) ,  ]
      nm = nm[which(!is.na( nm$id ) ) , ]
      
      if (nrow( nm) < 1 ) next()

      uid = sort( unique( nm$id)) 

      if ( !is.null( bottom.contact.debug.id ) ) {
        if (! bottom.contact.debug.id %in% uid) next()
        uid = bottom.contact.debug.id
      }

      ### if rerun necessary .. start from here until R-inla behaves more gracefully with faults

      if ( file.exists (fn.current) ) {
        # if there is something in the current id from a previous run, this indicates that this is likely a re-start
        # reload saved data and skip ahead to the next id
          cur = scan( fn.current, what="character", quiet=TRUE )
          if ( length(cur) > 0 ) {
            skip = which( uid==cur ) + 1
            uid = uid[ skip: length(uid) ]
            # load( fn.gsinf)  # as it is a restart ... load in the saved version instead of the initial version
          }
      }

      for ( id in uid) {
        print( id)
        if ( id %in% p$bc.badlist )  next()
        if ( file.exists (fn.current) ) {
          # if there is something in the current id from a previous run, this indicates that this is likely a re-start
          # add it to the list of "bad.list" and skip over for manual analysis
          cur = scan( fn.current, what="character", quiet=TRUE )
          if ( length(cur) > 0 ) {
            bad.list = NULL
            if (file.exists(fn.badlist) ) {
              bad.list = scan( fn.badlist, what="character", quiet=TRUE )
            }
            cat( unique( c(bad.list, cur)), file=fn.badlist )
          }
        }
        
        if ( file.exists (fn.badlist) ) {
          bad.list = scan( fn.badlist, what="character", quiet=TRUE )
          if ( id %in% bad.list ) next()
        }
        
        cat( id, file = fn.current )
        
        # id = "TEL2004529.21"

        ii = which( nm$id==id )  # rows of nm with scanmar/marport data
        if ( length( which( is.finite(nm[ii, "depth"]))) < 30 ) next() ## this will also add to the bad.list .. when insufficient data  
        gii = which( gsinf$id==id )  # row of matching gsinf with tow info
        if (length(gii) != 1) next()  # no match in gsinf  ## this will also add to the bad.list .. when insufficient data
       
        mm = nm[ which(nm$id==id) , c("depth", "timestamp") ]
         
        # NOTE:: do not use time.gate for historical data .. inconsistent timestamps causes data loss 
        # dropping time-gating as winch timestamps are too erratic and frequently wrong ... 
        # define time gate -20 from t0 and 50 min from t0, assuming ~ 30 min tow
        # time.gate = list( t0=gsinf$sdate[gii] - dminutes(20), t1=gsinf$sdate[gii] + dminutes(50) )

        # defaults appropriate for more modern scanmar data have > 3500 pings
        bcp = list( 
          id=id, datasource="groundfish", nr=nrow(mm), YR=YR,
          tdif.min=15, tdif.max=45,
          depth.min=10, setdepth=gsinf$bottom_depth[gii] 
        )
        
        bcp = bottom.contact.parameters( bcp ) # add other default parameters
        
        # over-ride stange data
        if (id=="NED2013028.172") {
          bcp$depth.range = c(-70, 70) 
        }
        if (id=="NED2013022.192"){
          bcp$depth.range = c(-250, 150) 
        }
     
        if (id=="NED2013022.193"){
          bcp$depth.range = c(-250, 150) 
        }


        bc = NULL # 
        bc = try( bottom.contact(mm, bcp ), silent=TRUE )
        
        if ( !is.null( bottom.contact.debug.id ) ) {
          ## this is a debugging mode return results and escape
          bottom.contact.plot( bc )
          return(bc)
        }

        if ( ! is.null(bc) && ( ! ( "try-error" %in% class(bc) ) ) ) { 
          bottom.contact.plot( bc )
          plotfn = file.path( scanmar.bc.dir, "figures", paste(id, "pdf", sep="." ) )
          print (plotfn)
          dev.flush()
          dev.copy2pdf( file=plotfn )
         
          if ( exists("error.flag", bc) && !is.na( bc$error.flag ) ) next()

          gsinf$bc0.datetime[gii] = bc$bottom0 
          gsinf$bc1.datetime[gii] = bc$bottom1
          gsinf$bottom_duration[gii] = bc$bottom.diff
          gsinf$bc0.sd[gii] = bc$bottom0.sd
          gsinf$bc1.sd[gii] = bc$bottom1.sd
          gsinf$bc0.n[gii] =  bc$bottom0.n
          gsinf$bc1.n[gii] =  bc$bottom1.n
          if ( !is.finite( gsinf$bottom_depth[gii]))  gsinf$bottom_depth[gii] = bc$depth.mean
          save (gsinf, file=fn.gsinf)  # temporary save in case of a restart in required for the next id
          fn.bc = file.path( scanmar.bc.dir, "results", paste( "bc", id, "rdata", sep=".") )  
          save ( bc, file=fn.bc, compress=TRUE )
        }
          
        # if only the loop completes without error, reset the flag for current id on filesystem
        cat("", file=fn.current )
      }

      ## END of re-run area ... 
      gsinf.tokeep = setdiff( names( gsinf), setdiff(gsinf0.names, "id") )
      gsinf = gsinf[ , gsinf.tokeep ]
      fn = file.path( scanmar.bc.dir, paste( "gsinf.bottom.contact", YR, "rdata", sep=".")  )
      save(gsinf, file=fn, compress= TRUE)
      
      print( "Troublesome id's have been stored in file:")
      print( fn.badlist )
      if (file.exists(fn.badlist)) print(  scan( fn.badlist, what="character", quiet=TRUE ) )

      if (file.exists(fn.current)) file.remove( fn.current )
      if (file.exists(fn.gsinf)) file.remove( fn.gsinf ) 
    
    }  # end for years

    return( YRS )
  }

   
  # -------
  

  if (DS %in% c("scanmar.filtered", "scanmar.filtered.redo", "scanmar.filtered.indices" )) {
    
    if (is.null (YRS) ) YRS = p$netmensuration.years 
  
    scanmar.filtered.dir =  file.path(p$scanmar.dir, "scanmar.filtered" )
    dir.create( scanmar.filtered.dir, recursive=TRUE, showWarnings=FALSE ) 
    dir.create (file.path( scanmar.filtered.dir, "results"), recursive=TRUE, showWarnings=FALSE )
    dir.create (file.path( scanmar.filtered.dir, "figures"), recursive=TRUE, showWarnings=FALSE )


    if(DS=="scanmar.filtered"){
      nm = NULL
      for ( YR in YRS ) {
        sc = scanmar.db( DS="sanity.checks", p=p, YRS=YR )
        ii = scanmar.db( DS="scanmar.filtered.indices", p=p, YRS=YR )
        if ( !is.null(sc) && !is.null(ii) && length(ii) > 0)  nm = rbind( nm, sc [ii,] )
      }
      return(nm)
    }


    if(DS=="scanmar.filtered.indices"){
      res = NULL
      for ( YR in YRS ) {
        out = NULL
        fn = file.path( scanmar.filtered.dir, paste( "scanmar.filtered.indices", YR, "rdata", sep=".")  )
        if (file.exists(fn)) load(fn) 
        res = c( res, out )
      }
      res = sort( unique( res))
      return(res)
    }
 
    nreq = 30
    sd.max = 30  # in seconds 
    counts = 0 

    for ( YR in YRS ) {
      out = NULL
      gs = scanmar.db( DS="bottom.contact", p=p, YRS=YR )
      if (is.null ( gs)) next()
      nm = scanmar.db( DS="sanity.checks", p=p, YRS=YR )
      if (is.null( nm)) next()
      nm$good = TRUE 
      nm$good[which(!is.finite(nm$depth)) ] = FALSE
      nm$good[which(is.na( nm$id ) )  ]   = FALSE
      w = which( nm$good) 
      if ( length(w) < 1 ) next()
      uid = unique( nm$id[w] )
      for ( id in uid) {
        # print( id)
        kk = jj = NULL
        kk = which( gs$id==id ) 
        jj = which( nm$id==id & nm$good )  # rows of nm with scanmar/marport data
        if (length( kk ) < 1) next()
        if (length( jj ) < nreq ) next()
        tk = which( nm$timestamp[jj] >= gs$bc0.datetime[kk] & nm$timestamp[jj] <= gs$bc1.datetime[kk] & nm$good[jj] )
        if (length(tk) < nreq ) next()
        ii = jj[tk]
        if ( length( which( is.finite(nm[ii, "depth"]))) < nreq ) next()  
        if ( all (is.finite( c( gs$bc0.sd[kk], gs$bc1.sd[kk] ) ))) {
          if ( gs$bc0.sd[kk] <= sd.max & gs$bc1.sd[kk] <= sd.max )  {  
            out = c(out, ii)
          }
        }
      }
      fn = file.path( scanmar.filtered.dir, paste( "scanmar.filtered.indices", YR, "rdata", sep=".")  )
      save( out, file=fn, compress= TRUE)
      nc = length( out)
      nu = length( uid ) 
      print( paste(fn, nc, nu) )
      counts = counts + nu
    } # end for years
    print( paste("Total count of unique id: ", counts ) ) 
    return( YRS)
  }


  # -----------------------------------


  if (DS %in% c("sweptarea", "sweptarea.redo" )) {
    
    if (is.null (YRS) ) YRS = p$netmensuration.years 
  
    scanmar.sa.dir =  file.path(p$scanmar.dir, "sweptarea" )
    dir.create( scanmar.sa.dir, recursive=TRUE, showWarnings=FALSE ) 
    dir.create (file.path( scanmar.sa.dir, "results"), recursive=TRUE, showWarnings=FALSE )
    dir.create (file.path( scanmar.sa.dir, "figures"), recursive=TRUE, showWarnings=FALSE )


    if(DS=="sweptarea"){
      out = NULL
      for ( YR in YRS ) {
        gs = NULL
        fn = file.path( scanmar.sa.dir, paste( "gsinf.sweptarea", YR, "rdata", sep=".")  )
        if (file.exists(fn)) load(fn) 
        out = rbind( out, gs )
      }
      return(out)
    }
  
    nreq = 30

    for ( YR in YRS ) {
   
      nm = scanmar.db( DS="scanmar.filtered", p=p, YRS=YR )
      if (is.null( nm)) next()
      uid = sort( unique( nm$id)) 

      gs = scanmar.db( DS="bottom.contact", p=p, YRS=YR )
      if (is.null ( gs)) next()
     
      gs$dist = NULL  # dummy values .. remove to avoid confusion 
      
      # get variable names and sequence of var's
      gstest = estimate.swept.area( getnames=TRUE )
      newvars = setdiff( gstest, names( gs) )
      for (vn in newvars) gs[,vn] = NA
      gs = gs[, names(gstest) ] # reorder

      if(FALSE) {
        # debugging 
        id="TEM2008775.22"
        id="NED2010001.59"
        ii = which( nm$id==id ) 
        gii = which( gs$id==id ) 
        gs = gs[gii,]
        x = nm[ii,]
      }
   
       
      for ( id in uid) {
        print( id)
        
        bc = scanmar.db( DS="bottom.contact.id", p=p, id=id )
        
        gii = jj = NULL
        jj  = which( nm$id==id )  # rows of nm with scanmar/marport data
        gii = which( gs$id==id )  # row of matching gsinf with tow info

        sa = estimate.swept.area( gsi=gs[gii,],  x= nm[jj,] )
        gs$sa[gii] = sa$surfacearea
        # gs$ ...

      }
      save(gs, file=fn, compress= TRUE)
    } # end for years
    return(YRS)
  } # end DS


} 



