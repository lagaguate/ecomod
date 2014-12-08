
net_mensuration.db=function( DS, nm=NULL, netswd=getwd() ){
   
  if(DS %in% c("perley.database", "perley.database.merge", "perley.database.datadump" )) {
    
    # fn1= old data, fn2= new data and fn3= merged data (old and new)
    fn1= file.path(netswd,"scanmar.perley.rdata")
    fn2= file.path(netswd,"scanmarnew.perley.rdata")
    fn3= file.path(netswd,"scanmar.perley.merged.rdata")
  
    if(DS=="perley.database.datadump"){
      # Package RODBC is a platform for interfacing R to database systems
      # Package includes the obdc* commands (access) and sql* functions (read, save, copy, and manipulate data between data frames)
      require(RODBC)
      connect=odbcConnect( "quoddy", uid="cooka", pwd="f43xy21b", believeNRows=F)
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
      
      # load groundfish inf table which has timestamps of start/stop times and locations
      gsinf = groundfish.db( DS="gsinf" )
      gsinfvars=c("id", "sdate", "time", "dist", "settype")
      
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
      
      
      #lubridate function 
      nm$timestamp = ymd_hms(nm$timestamp) 
      
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
    
      print ("The following have sets that cross midnight and require days to be adjusted" )
      nm$timestamp = timestamp.fix ( nm$id, nm$timestamp, threshold.hrs=2 )
      
      save(nm,file=fn3,compress=TRUE)
    }
  }
  
 

# Step to filter data  
 if(DS %in% c("sanity.checks", "sanity.checks.redo") ) {
   
   fn = file.path( netswd, "scanmar.sanity.checked.rdata")
   if(DS=="sanity.checks") {
     nm = NULL
     if (file.exists(fn)) load(fn)
     return(nm)
   }

   nm = net_mensuration.db( DS="merge.historical.scanmar", netswd=netswd ) 
   
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
   master$date=substring(master$timestamp,0,10) 
   
   save( nm, file=fn, compress=TRUE)
  }


  if(DS %in% c("post.perley", "post.perley.redo"))  {
    basedata=NULL
    fn=file.path( netswd, paste( "scanmar", "post.perley","rdata", sep="." ))
    if(DS == "post.perley"){
      if (file.exists(fn)) load(fn)
      return(basedata)
    }
    filelist = list.files(path=netswd, pattern="set.log", full.names=T, recursive=TRUE, ignore.case=TRUE)
    unneeded = grep ("copy", filelist, ignore.case=TRUE)
    if (length(unneeded>0)) filelist = filelist[-unneeded]
    for ( fl in filelist ) {
      print(fl)
      j = load.scanmar.rawdata( fl )  # variable naming conventions in the past
      if (is.null(j)) next()
      basedata = rbind( basedata, j)
    }
   save(basedata, file=fn, compress= TRUE)
  }



  if(DS %in% c("marport", "marport.redo"))  {
    basedata=NULL
    fn=file.path( netswd, paste( "marport", "rdata", sep="." ))
    if(DS == "marport"){
      if (file.exists(fn)) load(fn)
      return(basedata)
    }
    
    # configs
    filelist4 = list.files(path=netswd, pattern="^config.*.ini$", full.names=T, recursive=TRUE, ignore.case=TRUE)
    cfg = data.frame( configroot = dirname( filelist4 ), fn=filelist4, stringsAsFactors=FALSE )
    
    filelist1 = list.files(path=netswd, pattern=".log$", full.names=T, recursive=TRUE, ignore.case=TRUE)
    filelist2 = list.files(path=netswd, pattern=".gps$", full.names=T, recursive=TRUE, ignore.case=TRUE)
    filelist3 = list.files(path=netswd, pattern=".sgp$", full.names=T, recursive=TRUE, ignore.case=TRUE)
   
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
      basedata = rbind( basedata, j)
      basedata$mission= paste(basedata$Cruise, basedata$set, sep=".")
      basedata$mission=toupper(basedata$mission) 
      unique(basedata$mission)
      head(basedata)
      basedata$mission = gsub("_", "", basedata$mission)
      basedata$mission = gsub("-", "", basedata$mission)
      unique(basedata$mission)
      # continue here
    }
    save(basedata, file=fn, compress= TRUE)
  }
  

  if(DS %in% c("merge.historical.scanmar", "merge.historical.scanmar.redo" )) {
    
    fn= file.path(netswd,"all.historical.data.rdata")
    master=NULL
    if(DS=="merge.historical.scanmar"){
      if (file.exists(fn)) load(fn)
      return(master)
    }
    
    pp= net_mensuration.db( DS="post.perley", netswd=netswd ) 
    
    pp$uniqueid = pp$id
    pp$id = NULL
    pp$time = NULL
     
    nm = net_mensuration.db( DS="perley.database", netswd=netswd ) 
    w = which(!is.finite(nm$cspd))
    nm$ctspeed[w]=nm$cspd[w]
    v = which(!is.finite(nm$fspd))
    nm$ltspeed[v]=nm$fspd[v]
    v.to.drop = c("vesel", "empty", "logtime", "cspd", "fspd", "settype", "dist" )
    for ( v in v.to.drop) nm[,v] = NULL
    nm$gyro=NA  
    
    # here we will add the more modern data series and merge with perley
    meta = match.set.from.gpstrack(DS="post.perley", netswd=netswd )
    
    pp = merge(pp, meta, by="uniqueid", all.x=TRUE, all.y=FALSE)
    
    setdiff(names(pp), names(nm))
    pp$uniqueid=NULL
    pp$ctspeed=NA
    pp=pp[,names(nm)]
    # this is where we add the marport data/2010-2011 data
    master=rbind(nm, pp)
     
    save(master, file=fn, compress= TRUE)
    
  }


  if (DS %in% c("bottom.contact", "bottom.contact.redo" )) {
    
    fn= file.path(netswd,"gsinf.bottom.contact.rdata")
    master=NULL
    if(DS=="bottom.contact"){
      if (file.exists(fn)) load(fn)
      return(master)
    }
    
    gsinf = groundfish.db( DS="gsinf" )
    gsinf$bottom_duration = NA
    gsinf$spoint.datetime = as.POSIXct(NA)
    gsinf$epoint.datetime = as.POSIXct(NA)
    gsinf$spoint.sd = NA
    gsinf$epoint.sd = NA
    gsinf$epoint.n = NA
    gsinf$spoint.n = NA
     
    master = net_mensuration.db( DS="sanity.checks", netswd=netswd )
    
    uid = sort( unique( master$id)) 
    for ( id in uid) {
      print( id)
      ii = which( master$id==id )  # rows of master with scanmar/marport data
      if ( length( which( is.finite(master[ii, "depth"]))) < 30 ) next()  
      gii = which( gsinf$id==id )  # row of matching gsinf with tow info
      if (length(gii) != 1) next()  # no match in gsinf
      
      # vary the proportion of depth passed onto the bottom contact method as the shape of the tails can cause errors
      depthproportions = c(0.25, 0.5, 0.75, 0.9 ) 
      out = NULL
      alldata = list()
      for ( dp in depthproportions ) {
          res = bottom.contact.groundfish ( master[ii,c("depth", "timestamp")], depthproportion=dp, plot.data=TRUE)
          if (is.null(res)) next()
          tmp = cbind(  res$bottom0.sd,  res$bottom1.sd, sqrt(res$bottom0.sd^2 + res$bottom1.sd^2), 
            as.character(res$bottom0), as.character(res$bottom1), res$bottom0.n, res$bottom1.n, dp )  
          out = rbind( out, tmp )
          alldata[dp] = res 
          print (tmp)
      } 
      out = data.frame( out)
      colnames(out) = c("bottom0.sd", "bottom1.sd", "bottomall.sd", "bottom0", "bottom1", "bottom0.n", "bottom1.n", "depthproportion" )
      
      nums = c("bottom0.sd", "bottom1.sd", "bottomall.sd", "bottom0.n", "bottom1.n", "depthproportion" )
      out = factor2number ( out, nums )
      out$durations = as.numeric( ymd_hms( out$bottom1 ) - ymd_hms( out$bottom0 ) ) 
   
      good = which ( out$durations > 15 & out$durations < 45 ) 
      if (length(good) ==0 ) next() 
      out = out[ good,] 

      si = which.min( out[,"bottom0.sd"]) 
      ss = ymd_hms( out[si,"bottom0"] )
      
      ei = which.min( out[,"bottom1.sd"])
      ee = ymd_hms( out[ei,"bottom1"] )

      gsinf$spoint.datetime[gii] = as.character( ss )
      gsinf$epoint.datetime[gii] = as.character( ee )
      gsinf$bottom_duration[gii] = abs( as.numeric( diff( c(ee, ss) ) ))
      gsinf$spoint.sd[gii] = out[si,"bottom0.sd"]
      gsinf$epoint.sd[gii] =  out[ei,"bottom1.sd"] 
      gsinf$spoint.n[gii] = out[si,"bottom0.n"]
      gsinf$epoint.n[gii] = out[ei,"bottom1.n"] 
     
    }
    save(gsinf, file=fn, compress= TRUE)
  }

  

  if (DS %in% c("sweptarea", "sweptarea.redo" )) {
    
    fn= file.path(netswd,"gsinf.sweptarea.rdata")
    master=NULL
    if(DS=="sweptarea"){
      if (file.exists(fn)) load(fn)
      return(master)
    }
   
    master = net_mensuration.db( DS="sanity.checks", netswd=netswd )
    
    gsinf =  net_mensuration.db( DS="bottom.contact", netswd=netswd )
    gsinf$sweptarea.mean = NA 
    gsinf$depth.mean = NA
    gsinf$depth.sd = NA
    gsinf$wingspread.mean = NA
    gsinf$wingspread.sd = NA
     
    
    uid = sort( unique( master$id)) 
    for ( id in uid) {
      print( id)
      ii = which( master$id==id )  # rows of master with scanmar/marport data
      if ( length( which( is.finite(master[ii, "depth"]))) < 30 ) next()  
      gii = which( gsinf$id==id )  # row of matching gsinf with tow info
      if (length(gii) != 1) next()  # no match in gsinf
     
      gsinf[gii,] = estimate.swept.area( gsinf[gii,],  master[ii,] )
    }
    save(gsinf, file=fn, compress= TRUE)
  }
}
  # either & or | can be used to add conditions
