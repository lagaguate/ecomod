

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

  if(DS %in% c("perley", "perley.datadump", "perley.redo" )) {
    
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
      nm$nm_id = paste( "Perley", nm$mission, nm$setno, sep=".")
      
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
      
      tzone = "America/Halifax"  ## need to verify if this is correct ...  
  
      #lubridate function 
      nm$timestamp = ymd_hms(nm$tstamp)
      tz( nm$timestamp )=tzone

      keep=c("id", "nm_id", "vesel", "ltspeed", "ctspeed", "wingspread", "doorspread", "clearance",
             "opening", "fspd", "cspd", "latitude", "longitude", "depth", "settype", "timestamp", "yr"
             )
      nm=nm[,keep]

      # fix sets that cross midnight and list
      # some sets cross midnight and require days to be adjusted
      nm$timestamp = timestamp.fix (nm$timestamp, threshold.hrs=2 )
       
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

    varnames = c( "id", "nm_id", "ltspeed", "ctspeed", "wingspread", "doorspread", "clearance", "opening", 
                  "latitude", "longitude", "depth", "gyro", "timestamp")

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

        unneeded = grep ("aborted", filelist, ignore.case=TRUE)
        if (length(unneeded>0)) filelist = filelist[-unneeded]
   
        unneeded = grep ("rejected", filelist, ignore.case=TRUE)
        if (length(unneeded>0)) filelist = filelist[-unneeded]

        unneeded = grep ("gt\\.", filelist, ignore.case=TRUE)  # "gear trials"
        if (length(unneeded>0)) filelist = filelist[-unneeded]

        if (length( filelist) > 0 ) {
          print( "Reading in scanmar log files:" )
          for ( fl in filelist ) {
            print(fl)
            j = load.scanmar.rawdata( fl, yr=YR )  # variable naming conventions in the past  .. always UTC
            if (is.null(j)) next()
            j$ctspeed=NA  # missing in modern data so add to permit merging with historical data
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
        gf = NULL
        fnYR = file.path( p$scanmar.dir, "basedata.lookuptable", paste( "scanmar", "basedata.lookuptable", YR, "rdata", sep= "."))
        if ( file.exists(fnYR)) {
          load(fnYR)
          out = rbind( out, gf )
        }
      }
      return(out)
    }

    gf0 = groundfish.db(DS="gsinf")
    gf0$yr = lubridate::year( gf0$sdate)
    gf0$nm_id = NA
    gf0$nm_id0 = NA   # this stores the initial id  ... perley data and >=2015 data have hand matched data .. but many are not matched correctly ..
    gf0$timestamp = gf0$sdate
    gf0$min.distance = NA
    gf0$time.difference = NA
    gf0$depth.difference = NA
    gf0$match.level = NA  # -1 = not matched 
 

    for ( YR in YRS ) {
      fnYR = file.path( p$scanmar.dir, "basedata.lookuptable", paste( "scanmar", "basedata.lookuptable", YR, "rdata", sep= "."))
      skipyear = FALSE
      # Incorporation of newer data, combining timestamp
      
      igf = which( gf0$yr==YR)
      if (length(igf)==0) {
        skipyear = TRUE 
      } else {
        gf = gf0[igf,]
      }
         
      nm=scanmar.db( DS="basedata", p=p, YRS=YR ) 
      if (is.null( nm))  skipyear = TRUE 

      if ( ! skipyear ) {
        
        nm$lon=nm$longitude
        nm$lat=nm$latitude
        nm$longitude =NULL
        nm$latitude =NULL
   
        # match level 0 :: if there is already a misson.set identified in nm then record it 
        # ... but there seem to be errors so go through the exhaustive process as well 
        # store but do not use the id's ... they are unreliably matched (esp in the Perley data base) 
        # but have been identified more reliably starting in 2015
        nm$nm_id0 = nm$nm_id   

        # thin the nm data to speed up processing  .. only approximate locations and time stamps are required
        #keep = seq( from=1, to=nrow(nm), by=60 )  # i.e. ~ every 1 min
        #nm = nm[keep,]


        # now switching focus upon gsinf: if gs$nm_id is missing then we require a matching nm identity (filename) 
        # match level 1:: unique element with data within 1 km an 1 hr
        # criteria for exact matches in time (hr) and distance (km) :: 1 nm = 1.852 km, 3
        
        unmatched = which( is.na( gf$nm_id ) )
        if (length(unmatched)>0) {
          coords = c("lon", "lat")
          for(igg in unmatched) {
            # criteria for matching: distance, time and depth differences
            distance.km = try( abs( as.vector( geosphere::distCosine( nm[ , coords], gf[igg, coords])) )/1000 , silent=TRUE )
            diftime.hr = as.numeric( difftime( nm$timestamp, gf$sdate[igg], units="hours" ) )
            dd = which( abs(diftime.hr) <= 4  )  
            difdepth.m = nm$depth - gf$bottom_depth[igg]
            ddepth = which( abs(difdepth.m) < 20 )
     
            if ( length( dd) > 0 ) { 
              res = NULL  # container for matches
              # match 2: time-based only
              # ... lower condifence as there are so many issues with timezones and unsynced clocks
              fns =  unique( nm$nm_id[dd] ) 
              fn0 = NULL
              for ( fni in fns ) {
                ui = which( nm$nm_id[dd] == fni ) 
                jj = dd[ui]
                time.inside.interval = FALSE
                if (  gf$sdate[igg] >= min(nm$timestamp[jj]) & gf$sdate[igg] <= max(nm$timestamp[jj]) ) time.inside.interval = TRUE
                if (length( which( is.finite ( nm$depth[jj] ) ) ) > 30 ) {
                  dmode = modes( nm$depth[jj] )  # fast estimate of location of bottom
                  mm = which( nm$depth[jj] < dmode$ub2 & nm$depth[jj] > dmode$lb2 ) 
                  m = median( trunc(jj[mm]) ) # approx midpoint of bottom
                } else { 
                  m = median( trunc(jj) ) 
                }
                time.hr = abs( as.numeric( difftime( nm$timestamp[m], gf$sdate[igg], units="hours" ) ) )
                fno = NULL
                fno = data.frame( time.hr = time.hr )
                fno$distance.km=distance.km[m]
                fno$depth.diff = difdepth.m [m]
                fno$nm_id = fni
                fno$time.inside.interval = time.inside.interval
                fno$match.level = NA 
                res = rbind( res, fno )
              }
 
              # res contains best candidate matches, now compute best match choose best match, if any ...
              uu = which( !is.finite( res$time.hr))
              if ( length(uu) > 0) {
                res$time.hr[uu] = NA  # needs to be a separate step .. do not remove
                res$time.hr[uu] = min( 10, max(res$time.hr, na.rm=TRUE) )
              }
              
              vv = which( !is.finite( res$distance.km))
              if ( length(vv) > 0) {
                res$distance.km[vv] = NA # needs to be a separate step .. do not remove
                res$distance.km[vv] = min( 10, max(res$distance.km, na.rm=TRUE) )
              }

              oo = which(  res$time.inside.interval )
              if (length(oo) > 0) res$match.level[oo] = 0 +  res$distance.km[oo]     
    
              o0 = which( !is.finite(res$match.level) &&  res$time.hr < 0.5 & res$distance.km < 0.5 ) 
              if (length(o0) > 0) res$match.level[o0] = 0.5 + res$distance.km[o0] + res$time.hr[o0]

              o1 = which( !is.finite(res$match.level) &&  res$time.hr < 2 & res$distance.km < 2 ) 
              if (length(o1) > 0) res$match.level[o1] = 1 + res$distance.km[o1]  +  res$time.hr[o1]   

              o2 = which( !is.finite(res$match.level) && res$time.hr >= 2  & res$time.hr < 4 & res$distance.km < 2 ) 
              if (length(o2) > 0) res$match.level[o2] = 2 + res$time.hr[o2]  +  res$distance.km[o2] 

              o3 = which( !is.finite( res$match.level) ) 
              if (length(o3) > 0) res$match.level[o3] = 3 + res$distance.km[o3]  +  res$time.hr[o3] 
              
              o4 = which( !is.finite( res$match.level) ) 
              if (length(o4) >= 0) res$match.level[o4] = 3 + res$time.hr[o4]   # time only as depth is missing in perley db
              
              best.choice = which.min( res$match.level )  
              if ( length( best.choice ) > 0 ) {
                if ( res$match.level[best.choice]  < 5 ) {  
                  # 5 .. in case of time zone issues and some gps timing issues  ... anything higher is too unreliable
                  gf$match.level[igg] = res$match.level[best.choice]
                  gf$nm_id[igg] = res$nm_id[best.choice]
                  gf$timestamp[igg] = gf$sdate[igg] 
                  gf$min.distance[igg] = res[best.choice, "distance.km"]
                  gf$time.difference[igg] = res[best.choice, "time.hr"]
                  gf$depth.difference[igg] = res[best.choice, "depth.diff"] 
                }
              }
            }
          } # end for loop  unmatched
        }  # end if

        # remove duplicated nm_id's
        uu = which( duplicated( gf$nm_id, incomparables=NA ) ) 
        if ( length(uu) > 0 ) {
          print( "Duplicated matches:")
          while ( length( uu) > 0 )  {
            did = unique( gf$nm_id[uu] )
            hh = did[1]
            ee = which( gf$nm_id == hh )
            print( gf[ee,] )
            if (any( is.finite( gf$match.level[ee] ) ) ) {
              ff = which.min( gf$match.level[ee] )
              if ( is.finite(ff)) {
                gg = setdiff( ee, ee[ff] ) 
                gf$nm_id[gg] = NA
              }
            } else if ( any( is.finite( gf$min.distance[ee] ) ) ) {
              ff = which.min( gf$min.distance[ee] )
              if ( is.finite(ff)) {
                gg = setdiff( ee, ee[ff] ) 
                gf$nm_id[gg] = NA
              }
            } else if ( any( is.finite( gf$time.difference[ee] ) ) ) {
              ff = which.min( abs( gf$time.difference[ee] ))  # assuming time is correct ! which it is sometimes not ...
              if ( is.finite(ff)) {
                gg = setdiff( ee, ee[ff] ) 
                gf$nm_id[gg] = NA
              }
            }
            uu = which( duplicated( gf$nm_id, incomparables=NA ) )
          }
        }
        
      } # end if skipyear

      save(gf, file= fnYR, compress= TRUE)
      print(fnYR)
    }  # end loop for YRS

    return( YRS )
  }

  
  # -------------------------------------
 

  if ( DS %in% c("sanity.checks", "sanity.checks.redo") ) {
   # Step to merge and filter data  
    
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
   
      gf = NULL
      gf = scanmar.db( DS="basedata.lookuptable", p=p, YRS=YR  )
      if (is.null(gf)) next()

      gf = gf[, c("id", "nm_id", "sdate", "geardesc", "bottom_depth", "timestamp", "min.distance", "time.difference", "match.level" ) ]
      gf = gf[ which( !is.na( gf$nm_id) ) , ]
      
      # gate and filter the wingspread and door spread data  ... multi-pass quantile trimming
      # this is a global analysis ... all data required
    
      nm = scanmar.db( DS="basedata", p=p, YRS=YR ) 
      if (is.null(nm)) next()
         
      nmj = which( is.na( nm$nm_id) )  #required 
      if (length( nmj) > 0 ) nm = nm[ -nmj,]
 
      # drop a few vars that are not used and/or unreliable or unneeded
      nm$ltspeed = NULL
      nm$ctspeed = NULL
      nm$gyro = NULL
      nm$clearance = NULL
      nm$opening = NULL

      nmnrow0 = nrow( nm)
      nm = merge(nm, gf, by="nm_id", all.x=TRUE, all.y=FALSE, suffixes=c("", ".gf") )
      if( nrow(nm) != nmnrow0 ) stop("Merge error") 
  

      nmj = which( is.na( nm$id.gf ) )  # required ... sign that there is data that merged 
      if (length( nmj) > 0 ) nm = nm[ -nmj,]
      
      nm$id = nm$id.gf 

     # empty variable is not needed (crossed channel with doorspread), also values present look erroneous in NED2005001 1
      if (YR==2005) {
        i = which( nm$id=="NED2005001.1" )
        if (length(i) >0) {
          nm$doorspread[i] = NA
          nm$wingspread[i] = NA
          nm$clearance[i] = NA
          nm$opening[i] = NA
          nm$ltspeed[i] = NA
          nm$ctspeed[i] = NA
        }
      }

     # coarse level gating   
      
     # ID sets where American trawls were used for comparative surveys
     oo = which( nm$geardesc == "US 4 seam 3 bridle survey trawl" ) 
     if (length( oo) > 0 ) {
       ## additional gating goes here ... currently using the same rules .. 
       nm$doorspread[oo] = filter.nets("doorspread.range", nm$doorspread[oo] )
       nm$wingspread[oo]  = filter.nets("wingspread.range", nm$wingspread[oo] )
 #      nm$clearance[oo]  = filter.nets("clearance.range", nm$clearance[oo] )
 #      nm$opening[oo]  = filter.nets("opening.range", nm$opening[oo] )
       nm$depth[oo]  = filter.nets("depth.range", nm$depth[oo] )
     }

     pp = which( nm$geardesc == "Western IIA trawl" )
     if (length(pp) >0 ) {
       nm$doorspread[pp] = filter.nets("doorspread.range", nm$doorspread[pp] )
       nm$wingspread[pp]  = filter.nets("wingspread.range", nm$wingspread[pp] )
 #      nm$clearance[pp]  = filter.nets("clearance.range", nm$clearance[pp] )
 #      nm$opening[pp]  = filter.nets("opening.range", nm$opening[pp] )
       nm$depth[pp]  = filter.nets("depth.range", nm$depth[pp] )
     }

      probs =c(0.05, 0.95)
      
      # simple range gates
      bad = which( nm$wingspread < 2 | nm$wingspread > 25   ) 
      if (length(bad) > 0 ) nm$wingspread[ bad] = NA
      bad = which( nm$doorspreadspread < 5 | nm$doorspread > 100 ) 
      if (length(bad) > 0 ) nm$doorspread[ bad] = NA

      # quantile-based gates
      bad = which.quantile( nm$wingspread, probs=probs, inside=FALSE)
      if (length(bad) > 0 ) nm$wingspread[ bad] = NA

      bad = which.quantile( nm$doorspread, probs=probs, inside=FALSE)
      if (length(bad) > 0 ) nm$doorspread[ bad] = NA

      # door must be wider than wing
      bad = which (nm$doorspread < nm$wingspread )  
      if (length(bad) > 0 ) {
        nm$doorspread[ bad] = NA
        nm$wingspread[ bad] = NA
      }

      ## NOTE:: droppping data without a set match ... this may be bit sever as there is data for 
      ## forming doorspread/wingspread relationships but as they will not be computed and used in any manner ... dropping is OK

      bad = which( is.na( nm$nm_id )) 
      if (length(bad) > 0 ) {
        nm$doorspread[ bad] = NA
        nm$wingspread[ bad] = NA
      }

      good = which( is.finite( nm$doorspread + nm$wingspread + log(nm$depth) ) )
      if (length(good) > 30 ) {
        dw = lm( doorspread ~ as.factor( nm_id ) + wingspread + log(depth), data=nm[good,], na.action="na.exclude" )
        # hist(dw$residuals, "fd")
        ddresid = residuals( dw )
        bad = which.quantile( ddresid, probs=probs, inside=FALSE)
        if ( length( bad) > 0 )  {
          nm$wingspread[ good[bad] ] = NA
          nm$doorspread[ good[bad] ] = NA
        }
      } else {
        good = which( is.finite( nm$doorspread + nm$wingspread  ) )  # deoth missing for early series
        if (length(good) > 30 ) {
          dw = lm( doorspread ~ as.factor( nm_id ) + wingspread , data=nm[good,], na.action="na.exclude" )
          # hist(dw$residuals, "fd")
          ddresid = residuals( dw )
          bad = which.quantile( ddresid, probs=probs, inside=FALSE)
          if ( length( bad) > 0 )  {
            nm$wingspread[ good[bad] ] = NA
            nm$doorspread[ good[bad] ] = NA
          }
        }
      }
      
      gooddata = which( !is.na( nm$id))
      if (length( gooddata) ==0 ) {
        print( paste("No id's found for", YR ))
        next()
      }
      nm = nm[gooddata, ]
      todrop = grep( ".gf$", names(nm))
      if (length(todrop)>0) nm = nm[ ,-todrop]
      
      # check for unbelievable matches based upon distance: these should really be right on top of each other ... 
      distanceissues = which( nm$min.distance > 2 )   
      if (length( distanceissues ) >0 ) {
        uu = unique( nm$id[distanceissues] )
        for (u in uu ) {
          vv = which( nm$id==u )
          nm$nm_id[vv] = NA  # reject the match
        }
        oo = which( is.na( nm$nm_id) )
        if (length( oo) >0 ) nm = nm[ - oo, ] 
      }

      # check for time zone type issues and assume gf is correct time
      timeissues = which( nm$time.difference > 2 ) # time.difference is in hours
      if (length(timeissues) >0 ) {
        uu = unique( nm$id[ timeissues ])
        for (u in uu ) {
          vv = which( nm$id==u )
          nm$timestamp[vv] = nm$timestamp[vv] + round( nm$time.difference[vv], 0 )
        }
      }
      nm$date = substring(as.character(nm$timestamp), 1,10)

      ids = strsplit( nm$id, "[.]" )
      
      mission.trip = unlist( lapply( ids, function(x){x[[1]]} ) )
      setno = unlist( lapply( ids, function(x){x[[2]]} ) )
      
      nm$set = NA
      nm$set = as.numeric( setno )
      
      nm$trip = NA
      nm$trip = substring( mission.trip, 8,10 )
      nm$trip = as.numeric(nm$trip)
      nm$year= lubridate::year(nm$timestamp)  

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

    gsinf0$bottom_duration = NA
    gsinf0$bc0.datetime = as.POSIXct(NA)
    gsinf0$bc1.datetime = as.POSIXct(NA)
    gsinf0$bc0.sd = NA
    gsinf0$bc1.sd = NA
    gsinf0$bc0.n = NA
    gsinf0$bc1.n = NA
    gsinf0$door.sa = NA
    gsinf0$wing.sa = NA
    gsinf0$door.mean = NA
    gsinf0$wing.mean = NA
    gsinf0$door.sd =  NA
    gsinf0$wing.sd =  NA

    for ( YR in YRS ) {
      
      skipyear = FALSE

      yy = which( gsinf0$year == YR )
      if (length(yy)==0 ) {
        skipyear = TRUE 
      } else {
        gsinf = gsinf0[ yy, ]  
      }

      nm = scanmar.db( DS="sanity.checks", p=p, YRS=YR )
      if (is.null( nm)) {
        skipyear = TRUE 
      } else {
        nm = nm[which(is.finite(nm$depth)) ,  ]
        nm = nm[which(!is.na( nm$id ) ) , ]
      }

      if (nrow( nm) < 1 ) {
        skipyear = TRUE 
      }
      
      uid = unique( nm$id)

      ui = which (!is.na( uid) )
      if ( length( ui) == 0 ) {
        skipyear=TRUE
      } else {
        uid = sort( uid[ ui  ] )
      }
     
      if ( !is.null( bottom.contact.debug.id ) & length(uid)>0 ) {
          if (! bottom.contact.debug.id %in% uid) {
            skipyear = TRUE 
          } else { 
            uid = bottom.contact.debug.id
          }
      }

      if ( ! skipyear ) {

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


          ii = which( nm$id==id )  # rows of nm with scanmar/marport data
          if ( length( which( is.finite(nm[ii, "depth"]))) < 30 ) next() ## this will also add to the bad.list .. when insufficient data  
          gii = which( gsinf$id==id )  # row of matching gsinf with tow info
          if (length(gii) != 1) next()  # no match in gsinf  ## this will also add to the bad.list .. when insufficient data
         
          mm = nm[ which(nm$id==id) ,]
           
          # NOTE:: do not use time.gate for historical data .. inconsistent timestamps causes data loss 
          # dropping time-gating as winch timestamps are too erratic and frequently wrong ... 
          # define time gate -20 from t0 and 50 min from t0, assuming ~ 30 min tow
          # time.gate = list( t0=gsinf$sdate[gii] - dminutes(20), t1=gsinf$sdate[gii] + dminutes(50) )

          # defaults appropriate for more modern scanmar data have > 3500 pings
          bcp = list( 
            id=id, datasource="groundfish", nr=nrow(mm), YR=YR,
            tdif.min=13, tdif.max=45, setdepth=gsinf$bottom_depth[gii] 
          )
          
          bcp = bottom.contact.parameters( bcp ) # add other default parameters
          
          # low-level over-ride of bottom contact parameters for strange data
          if (id=="NED2013028.172") bcp$depth.range = c(-70, 70) 
          if (id=="NED2013022.192") bcp$depth.range = c(-300, 300) # not sure why this has such a large range! 
          if (id=="NED2013022.193") bcp$depth.range = c(-250, 150) 
          if (id=="TEL2004529.16")  bcp$depth.range = c(-150, 150) 


          if (YR == 2015) {
            # two depth sensors were used simultaneously but they are not calibrated! 
            double.depth.sensors = paste( "NED2015002", c( 51:54, 55:64), sep="." )  
            if ( id %in% double.depth.sensors ) bcp$double.depth.sensors = TRUE 
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
            
            if ( ! exists("error.flag", bc) || is.na( bc$error.flag ) ) { 
              gsinf$bc0.datetime[gii] = bc$bottom0 
              gsinf$bc1.datetime[gii] = bc$bottom1
              gsinf$bottom_duration[gii] = bc$bottom.diff
              gsinf$bc0.sd[gii] = bc$bottom0.sd
              gsinf$bc1.sd[gii] = bc$bottom1.sd
              gsinf$bc0.n[gii] =  bc$bottom0.n
              gsinf$bc1.n[gii] =  bc$bottom1.n
             
              if ( exists("surface.area", bc) ) { 
                if ( is.list( bc$surface.area )  && !is.na( bc$surface.area )  ) {
                  if ( exists("door.sa", bc$surface.area ) ) gsinf$door.sa[gii] =  bc$surface.area$door.sa
                  if ( exists("wing.sa", bc$surface.area ) ) gsinf$wing.sa[gii] =  bc$surface.area$wing.sa
                  if ( exists("door.mean", bc$surface.area ) ) gsinf$door.mean[gii] =  bc$surface.area$door.mean
                  if ( exists("wing.mean", bc$surface.area ) )  gsinf$wing.mean[gii] =  bc$surface.area$wing.mean
                  if ( exists("door.sd", bc$surface.area ) ) gsinf$door.sd[gii] =  bc$surface.area$door.sd
                  if ( exists("wing.sd", bc$surface.area ) ) gsinf$wing.sd[gii] =  bc$surface.area$wing.sd
                }
              }
            }
       
            if ( exists("depth.mean", bc) && !is.finite( bc$depth.mean)) gsinf$bottom_depth[gii] = bc$depth.mean  # over ride as there are many issues with the depth recorded in gsinf ...
            save (gsinf, file=fn.gsinf)  # temporary save in case of a restart in required for the next id
            fn.bc = file.path( scanmar.bc.dir, "results", paste( "bc", id, "rdata", sep=".") )  
            save ( bc, file=fn.bc, compress=TRUE )
          }
            
          # if only the loop completes without error, reset the flag for current id on filesystem
          cat("", file=fn.current )
        }
      }

      ## END of re-run area ... 
      gsinf.tokeep = setdiff( names( gsinf), setdiff(gsinf0.names, "id") )
      gsinf = gsinf[ , gsinf.tokeep ]
      fn = file.path( scanmar.bc.dir, paste( "gsinf.bottom.contact", YR, "rdata", sep=".")  )
      save(gsinf, file=fn, compress= TRUE)
   
    }  # end for years

    print( "Troublesome id's have been stored in file:")
    print( fn.badlist )
    if (file.exists(fn.badlist)) print(  scan( fn.badlist, what="character", quiet=TRUE ) )

    if (file.exists(fn.current)) file.remove( fn.current )
    if (file.exists(fn.gsinf)) file.remove( fn.gsinf ) 
 
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
  
} 



