
net_mensuration.db=function( DS, nm=NULL, net.root.dir=file.path( project.directory("groundfish"), "data", "nets" ), user.interaction=FALSE, override.missions=NULL ){
  
  scanmar.dir = file.path( net.root.dir, "Scanmar" )
  marport.dir = file.path( net.root.dir, "Marport" )

  if (!file.exists( scanmar.dir )) {
   mk.Dir = readline("Directory not found, shall we create it? Yes/No (Default No) : ")
    if ( mk.Dir =="Yes" ) {
      dir.create( scanmar.dir, recursive=TRUE) 
      print( paste("The directory -- ",  scanmar.dir, " -- has been created") )
    } else {
      warning( paste("Directory: ", scanmar.dir, " was not found." )) 
    }
  }

  if (!file.exists( marport.dir )) {
    mk.Dir = readline("Directory not found, shall we create it? Yes/No (Default No) : ")
    if ( mk.Dir =="Yes" ) {
      dir.create( marport.dir, recursive=TRUE) 
      print( paste("The directory -- ",  marport.dir, " -- has been created") )
    } else {
      warning( paste("Directory: ", marport.dir, " was not found." )) 
    }
  }


  if(DS %in% c("perley.database", "perley.database.merge", "perley.database.datadump" )) {
    
    # fn1= old data, fn2= new data and fn3= merged data (old and new)
    fn1= file.path(scanmar.dir,"scanmar.perley.rdata")
    fn2= file.path(scanmar.dir,"scanmarnew.perley.rdata")
    fn3= file.path(scanmar.dir,"scanmar.perley.merged.rdata")
  
    if(DS=="perley.database.datadump"){
      # Package RODBC is a platform for interfacing R to database systems
      # Package includes the obdc* commands (access) and sql* functions (read, save, copy, and manipulate data between data frames)
      require(RODBC)
      connect=odbcConnect( oracle.perley.db, uid=oracle.perley.user, pwd=oracle.perley.password, believeNRows=F)
      # sqlquery can be used to return part of a table
      scanmar = sqlQuery(connect, "select * from   groundfish.perleyp_SCANMAR", as.is=T) 
      scanmarnew = sqlQuery(connect, "select * from   groundfish.perleyp_NEWSCANMAR", as.is=T) 
      # closing the connection with ODBC
      odbcClose(connect)
      # saving the tables to R memory in zip format 
      save(scanmar, file=fn1, compress=T)
      save(scanmarnew, file=fn2, compress=T)
    }
    
    # Need an explanation of this step
    if(DS=="perley.database"){
      nm = NULL
      if (file.exists(fn3)) load(fn3)
      return(nm)
    }
  
    # Changing scanmarnew names to lowercase 
    if(DS=="perley.database.merge"){
      load(fn1) 
      load(fn2)
      names(scanmarnew)=tolower(names(scanmarnew))      
      names(scanmar)=tolower(names(scanmar))      #ac
      
      # nm is the dataset which combines the old and new data (merged)
      # some variables were missing from scanmar to faciliate the merge of scanmarnew
      nm=scanmar
      nm$fspd=NA
      nm$cspd=NA        
      nm$latitude=NA
      nm$longitude=NA
      nm$depth=NA
      nm$empty=NA
      
      rm(scanmar)

      # using logtime to create the time variable
      scanmarnew$logtime=scanmarnew$time
      scanmarnew$time=NULL
      
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
      nm$year= as.numeric(substring(nm$mission,4,7))
      nm$fspd=as.numeric(nm$fspd)
      nm$cspd= as.numeric(nm$cspd)
      
      
      # merge groundfish  timestamps and ensure that net mensuration timestamps are correct
      
      nm$id=paste(nm$mission, nm$setno, sep=".")
      
      # Correcting for data which contains NA in the time slot by identifying and deleting it
      strangedata = which(is.na(nm$logtime))
      if(length(strangedata)>0) nm=nm[-strangedata,]

      ii = which( nm$longitude > 0 )
      if (length(ii) > 0 ) nm$longitude[ii] = - nm$longitude[ii] 
      
      # load groundfish inf table which has timestamps of start/stop times and locations
      gsinf = groundfish.db( DS="gsinf" )
            
      gsinfvars=c("id", "sdate", "edate", "time", "settype" )
      
      # merge 
      nm = merge( nm, gsinf[,gsinfvars], by="id", suffixes=c(".nm", ""), all.x=TRUE, all.y=FALSE)
      
      # fix some time values that have lost the zeros due to numeric conversion
      nm$logtime=gsub(":", "", nm$logtime)      
      j=nchar(nm$logtime)
      tooshort=which(j==5)
      nm$logtime[tooshort]=paste("0",nm$logtime[tooshort],sep="")
      
      tooshort=which(j==4)
      nm$logtime[tooshort]=paste("00",nm$logtime[tooshort],sep="")
      
      tooshort=which(j==3)
      nm$logtime[tooshort]=paste("000",nm$logtime[tooshort],sep="")
      
      tooshort=which(j==2)
      nm$logtime[tooshort]=paste("0000",nm$logtime[tooshort],sep="")
      
      tooshort=which(j==1)
      nm$logtime[tooshort]=paste("00000",nm$logtime[tooshort],sep="")
      
      nm$hours=substring(nm$logtime,1,2)
      
      nm$min=substring(nm$logtime,3,4)
      
      nm$sec=substring(nm$logtime,5,6)
      
      nm$day = day( nm$sdate )
      nm$mon = month( nm$sdate )
      
      i=which(!is.finite(nm$day))
      nm = nm[ -i, ]
      
      nm$timestamp= paste(nm$year,nm$mon, nm$day, nm$hours, nm$min, nm$sec, sep="-" )
      
      tzone = "America/Halifax"  ## need to verify if this is correct
  
      #lubridate function 
      nm$timestamp = ymd_hms(nm$timestamp, tz=tzone) 
      
      nm$uniqueid=NULL
      nm$mission=NULL
      nm$cruno=NULL
      nm$setno=NULL
      nm$year=NULL
      nm$day=NULL
      nm$mon=NULL
      nm$hours=NULL
      nm$min=NULL
      nm$sec=NULL
      nm$sdate=NULL
      nm$time=NULL
      
      # fix sets that cross midnight and list
      uniqueid = unique(nm$id)
    
      # some sets cross midnight and require days to be adjusted
      nm$timestamp = timestamp.fix (nm$timestamp, threshold.hrs=2 )
      
      save(nm,file=fn3,compress=TRUE)
    }
  }
  
 


  if(DS %in% c("post.perley", "post.perley.redo"))  {
    
    tzone = "America/Halifax"  ## need to verify if this is correct
    basedata=NULL
    
    fn=file.path( scanmar.dir, paste( "scanmar", "post.perley","rdata", sep="." ))
    if(DS == "post.perley"){
      if (file.exists(fn)) load(fn)
      return(basedata)
    }
    filelist = list.files(path=scanmar.dir, pattern="set.log", full.names=T, recursive=TRUE, ignore.case=TRUE)
    unneeded = grep ("copy", filelist, ignore.case=TRUE)
    if (length(unneeded>0)) filelist = filelist[-unneeded]
    for ( fl in filelist ) {
      print(fl)
      j = load.scanmar.rawdata( fl, tzone=tzone )  # variable naming conventions in the past
      if (is.null(j)) next()
      basedata = rbind( basedata, j)
    }
     tz(basedata$timestamp) = tzone
   save(basedata, file=fn, compress= TRUE)
  }


  if(DS %in% c("marport", "marport.redo"))  {
    basedata=NULL
    fn=file.path( marport.dir, paste( "marport", "rdata", sep="." ))
    if(DS == "marport"){
      if (file.exists(fn)) load(fn)
      return(basedata)
    }
    
    # configs
    # marport.dir = file.path("C:", "Users", "MundenJ", "Desktop", "Marport")
    filelist4 = list.files(path=marport.dir, pattern="^config.*.ini$", full.names=T, recursive=TRUE, ignore.case=TRUE)
    cfg = data.frame( configroot = dirname( filelist4 ), fn=filelist4, stringsAsFactors=FALSE )
    
    filelist1 = list.files(path=marport.dir, pattern=".log$", full.names=T, recursive=TRUE, ignore.case=TRUE)
    filelist2 = list.files(path=marport.dir, pattern=".gps$", full.names=T, recursive=TRUE, ignore.case=TRUE)
    filelist3 = list.files(path=marport.dir, pattern=".sgp$", full.names=T, recursive=TRUE, ignore.case=TRUE)
   
    filelist1 = gsub( ".log$", "", filelist1)
    filelist2 = gsub( ".gps$", "", filelist2)
    filelist3 = gsub( ".sgp$", "", filelist3)
    
    fileroots = unique( c( filelist1, filelist2, filelist3 ) )
    filelist = data.frame( fileroots=fileroots, configroot = dirname( fileroots ), stringsAsFactors=FALSE )
    filelist = merge( filelist, cfg, by="configroot", all.x=TRUE, all.y=FALSE )
    
    no.files = nrow(filelist)
    for ( ii in 1:no.files ) {
      fl = filelist$fileroots[ii]
      sensorconfig = filelist$fn[ii]
      print(fl)
      j = load.marport.rawdata( fl, sensorconfig )  # variable naming conventions in the past
      if (is.null(j)) next()
      j$rootname=fl
      basedata = rbind( basedata, j)
    }
    
    # Include mission as a variable (also trip and year)
    g=substring(basedata$rootname,54,63)      
    basedata$mission = paste(g, basedata$set, sep=".")
    basedata$year = substring(basedata$mission, 4,7)
    basedata$year=as.numeric(basedata$year)
    basedata$trip = substring(basedata$mission, 8,10)
    basedata$trip=as.numeric(basedata$trip)
    
    save(basedata, file=fn, compress= TRUE)
  }

  if(DS %in% c("marport.gated", "marport.gated.redo"))  {
    nm=NULL
    fn=file.path( marport.dir, paste( "marport.gated", "rdata", sep="." ))
    if(DS == "marport.gated"){
      if (file.exists(fn)) load(fn)
      return(nm)
    }
      nm = net_mensuration.db( DS="marport",  net.root.dir=net.root.dir ) # QA/QC of data
        
      nm$doorspread = filter.nets("doorspread.range", nm$doorspread)
      nm$wingspread = filter.nets("wingspread.range", nm$wingspread)
      nm$clearance = filter.nets("clearance.range", nm$clearance)
      nm$opening.scanmar = filter.nets("opening.range", nm$opening.scanmar)
      nm$depth = filter.nets("depth.range", nm$depth)
    
    save(nm, file=fn, compress=TRUE)
   
  }

  if(DS %in% c("merge.historical.scanmar", "merge.historical.scanmar.redo" )) {
    
    fn= file.path(scanmar.dir,"all.historical.data.rdata")
    master=NULL
    if(DS=="merge.historical.scanmar"){
      if (file.exists(fn)) load(fn)
      return(master)
    }
    
    pp= net_mensuration.db( DS="post.perley", net.root.dir=net.root.dir ) 
    
    pp$uniqueid = pp$id
    pp$id = NULL
    pp$time = NULL
     
    nm = net_mensuration.db( DS="perley.database", net.root.dir=net.root.dir ) 
    nm$netmensurationfilename = "Perley Oracle instance"
    w = which(!is.finite(nm$cspd))
    nm$ctspeed[w]=nm$cspd[w]
    v = which(!is.finite(nm$fspd))
    nm$ltspeed[v]=nm$fspd[v]
    v.to.drop = c("vesel", "empty", "logtime", "cspd", "fspd", "settype", "dist" )
    for ( v in v.to.drop) nm[,v] = NULL
    nm$gyro=NA  
    nm$edate = NULL
    
    # here we will add the more modern data series and merge with perley
    meta = match.set.from.gpstrack(DS="post.perley", net.root.dir=net.root.dir )
    
    pp = merge(pp, meta, by="uniqueid", all.x=TRUE, all.y=FALSE)
    
    pp$netmensurationfilename = pp$uniqueid 
    pp$uniqueid=NULL
    pp$ctspeed=NA
    
    # setdiff(names(nm), names(pp))
    pp=pp[,names(nm)]
    
    # this is where we add the marport data/2010-2011 data
    master=rbind(nm, pp)
    
    master$date = substring(as.character(master$timestamp), 1,10)
    gooddata = which( !is.na( master$id))
    
    
    ids = strsplit( master$id[gooddata], "[.]" )
    
    mission.trip = unlist( lapply( ids, function(x){x[[1]]} ) )
    setno = unlist( lapply( ids, function(x){x[[2]]} ) )
    
    master$set = NA
    master$set[gooddata] = as.numeric( setno )
    
    master$trip = NA
    master$trip[gooddata] = substring( mission.trip, 8,10 )
    master$trip = as.numeric(master$trip)
    master$year=year(master$timestamp)  
    
    save(master, file=fn, compress= TRUE)
    
  }


  if ( DS %in% c("sanity.checks", "sanity.checks.redo") ) {
   # Step to filter data  
   
   fn = file.path( scanmar.dir, "scanmar.sanity.checked.rdata")
   if(DS=="sanity.checks") {
     nm = NULL
     if (file.exists(fn)) load(fn)
     return(nm)
   }

   nm = net_mensuration.db( DS="merge.historical.scanmar", net.root.dir=net.root.dir ) 
   
   # remove sets where american trawls were used for comparative surveys
   nm = filter.nets("remove.trawls.with.US.nets", nm)
   # empty variable is not needed (crossed channel with doorspread), also values present look erroneous in NED2005001 1
   i = which( nm$id=="NED2005001.1" )
   nm$doorspread[i] = NA
   nm$wingspread[i] = NA
   nm$clearance[i] = NA
   nm$opening[i] = NA
   nm$ltspeed[i] = NA
   nm$ctspeed[i] = NA
   
   # coarse level gating   
   nm$doorspread = filter.nets("doorspread.range", nm$doorspread)
   nm$wingspread = filter.nets("wingspread.range", nm$wingspread)
   nm$clearance = filter.nets("clearance.range", nm$clearance)
   nm$opening = filter.nets("opening.range", nm$opening)
   nm$depth = filter.nets("depth.range", nm$depth)
   nm$door.and.wing.reliable = filter.nets( "door.wing", nm )    # flag to ID data that are bivariately stable .. errors still likely present
 
   save( nm, file=fn, compress=TRUE)
  }


  if (DS %in% c("bottom.contact", "bottom.contact.redo" )) {
    
    # scanmar.dir = file.path( net.root.dir, "Scanmar" )
    
    fn= file.path(scanmar.dir,"gsinf.bottom.contact.rdata" )
    master=NULL
    if(DS=="bottom.contact"){
      if (file.exists(fn)) load(fn)
      return(master)
    }
    
    gsinf = groundfish.db( DS="gsinf" )
    gsinf$bottom_duration = NA
    gsinf$bc0.datetime = as.POSIXct(NA)
    gsinf$bc1.datetime = as.POSIXct(NA)
    gsinf$bc0.sd = NA
    gsinf$bc1.sd = NA
    gsinf$bc0.n = NA
    gsinf$bc1.n = NA
    
    master = net_mensuration.db( DS="sanity.checks", net.root.dir=net.root.dir )
    master = master[which(master$year >= 2004) ,  ]
    
    if ( !is.null( override.missions)){
      user.interaction = TRUE
      master = master[ which(master$id %in% override.missions ), ]
    }
    

    fn.current = file.path( scanmar.dir, "bottom.contact.tmp.current" )
    fn.badlist = file.path( scanmar.dir, "bottom.contact.badlist" )
    fn.gsinf = file.path( scanmar.dir, "bottom.contact.tmp.gsinf" )

    badlist = skip = cur = NULL
    
    uid = sort( unique( master$id)) 

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

      ii = which( master$id==id )  # rows of master with scanmar/marport data
      if ( length( which( is.finite(master[ii, "depth"]))) < 30 ) next() ## this will also add to the bad.list .. when insufficient data  
      gii = which( gsinf$id==id )  # row of matching gsinf with tow info
      if (length(gii) != 1) next()  # no match in gsinf  ## this will also add to the bad.list .. when insufficient data
     
      mm = master[ which(master$id==id) , c("depth", "timestamp") ]

      # define time gate -20 from t0 and 50 min from t0, assuming ~ 30 min tow
      time.gate = list( t0=gsinf$sdate[gii] - dminutes(20), t1=gsinf$sdate[gii] + dminutes(50) )
      
      # x=mm; depthproportion=0.6; tdif.min=15; tdif.max=45; eps.depth=4; sd.multiplier=5; depth.min=10; depth.range=c(-50, 50); smoothing = 0.9; filter.quants=c(0.025, 0.975); plot.data=TRUE
      
      bc = NULL # 
      bc = try( bottom.contact(id, mm, depthproportion=0.6, tdif.min=15, tdif.max=45, eps.depth=4, sd.multiplier=5, 
                 depth.min=10, depth.range=c(-50, 50), smoothing = 0.9, filter.quants=c(0.025, 0.975), 
                 plot.data=TRUE, time.gate=time.gate ), 
          silent=TRUE)

      if ( ! is.null(bc) && ( ! ( "try-error" %in% class(bc) ) ) ) { 
        gsinf$bc0.datetime[gii] = bc$bottom0 
        gsinf$bc1.datetime[gii] = bc$bottom1
        gsinf$bottom_duration[gii] = bc$bottom.diff
        gsinf$bc0.sd[gii] = bc$bottom0.sd
        gsinf$bc1.sd[gii] = bc$bottom1.sd
        gsinf$bc0.n[gii] =  bc$bottom0.n
        gsinf$bc1.n[gii] =  bc$bottom1.n
        if ( !is.finite( gsinf$bottom_depth[gii]))  gsinf$bottom_depth[gii] = bc$depth.mean
        save (gsinf, file=fn.gsinf)  # temporary save in case of a restart in required for the next id
      }
        
      # if only the loop completes without error, reset the flag for current id on filesystem
      cat("", file=fn.current )
    }
   


    if (!is.null( override.missions)) {
      fn = paste( fn, "manually.determined.rdata", sep="")
      print( "Saving to an alternate location as this is being manually handled ... merge this by hand:" )
      print( fn)
    }
    
    save(gsinf, file=fn, compress= TRUE)
    
    print( "Use, override.mission=c(...) as a flag and redo .. manually ) for the following:" )
    print( "Troublesome id's have been stored in file:")
    print( fn.badlist )
    print(  scan( fn.badlist, what="character", quiet=TRUE ) )

    file.remove( fn.current )
  }

  

  if (DS %in% c("sweptarea", "sweptarea.redo" )) {
     
    # scanmar.dir = file.path( net.root.dir, "Scanmar" )

    fn= file.path(scanmar.dir,"gsinf.sweptarea.rdata")
    master=NULL
    if(DS=="sweptarea"){
      if (file.exists(fn)) load(fn)
      return(master)
    }
   
    master = net_mensuration.db( DS="sanity.checks", net.root.dir=net.root.dir )
    master = master[which(master$year >= 2004) ,  ]
    
    uid = sort( unique( master$id)) 
    
    #gsinf = groundfish.db( DS="gsinf" ) # to debug
    gsinf =  net_mensuration.db( DS="bottom.contact", net.root.dir=net.root.dir )
    gsinf$dist = NULL
    
    gsinf.odbc = groundfish.db( DS="gsinf.odbc" ) # loading raw data from Oracle data dump
    gsinf.odbc$id = paste( gsinf.odbc$mission, gsinf.odbc$setno, sep="." )
    updated.data = c("id", "speed", "dist")
    gsinf = merge( gsinf, gsinf.odbc[,updated.data], by="id", all.x=TRUE, all.y=TRUE )
    
    # get variable names and sequence of var's
    gstest = estimate.swept.area( getnames=TRUE )
    newvars = setdiff( gstest, names( gsinf) )
    for (vn in newvars) gsinf[,vn] = NA
    gsinf = gsinf[, names(gstest) ] # reorder

    if(FALSE) {
      id="TEM2008775.22"
      id="NED2010001.59"
      ii = which( master$id==id ) 
      gii = which( gsinf$id==id ) 
      gs = gsinf[gii,]
      x = master[ii,]
    }
    
    for ( id in uid) {
      print( id)
      ii = which( master$id==id )  # rows of master with scanmar/marport data
      if ( length( which( is.finite(master[ii, "depth"]))) < 30 ) next()  
      gii = which( gsinf$id==id )  # row of matching gsinf with tow info
      if (length(gii) != 1) next()  # no match in gsinf
      if ( all (is.finite( c( gsinf$bc0.sd, gsinf$bc1.sd ) ))) {
      if ( gsinf$bc0.sd > 30 & gsinf$bc1.sd > 30 )  {
        # SD of start and end times must have a convengent solution which is considered to be stable when SD < 30 seconds
        sa = estimate.swept.area( gsinf[gii,],  master[ii,] )
        gsinf$sa[gii] = sa$surfacearea
        # ... 
      }}
    }
    save(gsinf, file=fn, compress= TRUE)
  }
}


