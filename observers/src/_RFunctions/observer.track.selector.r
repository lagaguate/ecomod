observer.track.selector<-function( sought=NULL, gear=NULL,
                                 date.range.start=NULL,
                                 date.range.end=NULL){
  library(sqldf)
  sought=NULL
  soughtQ=""
  gear=NULL
  gearQ=""
  caught=NULL
  catchQ=""
  name=""
  catchfield=""
  catchtable=""
  catchjoin=""
  
    # Choose Gear or Species --------------------------------------------------
    level.1<-select.list(c("By Species Sought","By Species Caught","By Gear"),
                         multiple=F, graphics=T, 
                         title="Data View?")
    if (level.1=="By Species Sought" | level.1==""){ 
      #if left blank, we assume species
      focus="spp"
      the.species<-get.species()
      sought.GUI<-select.list(paste( the.species$COMMON, " (", the.species$SPECSCD_ID,")",sep=""),
                              multiple=T, graphics=T, 
                              title="Choose a species")
      sought <-SQL.in(as.numeric(gsub('.+\\(([0-9]+)\\).*?$', '\\1', sought.GUI)))
      soughtQ<-paste0("AND f.specscd_id IN (",sought,")")
      
      name<-if(length(sought)>1) paste0("spp_",sought[1],"_etc") else paste0("sp_",sought[1])
    }else if (level.1=="By Species Caught"){
      focus="caught"
      the.caught.species<-get.caught.species()
      caught.GUI<-select.list(paste( the.caught.species$COMMON, " (", the.caught.species$SPECCD_ID,")",sep=""),
                             multiple=T, graphics=T, 
                             title="Choose a species")
      
      caught <-SQL.in(as.numeric(gsub('.+\\(([0-9]+)\\).*?$', '\\1', caught.GUI)))
      catchfield=", ISCATCHES.SPECCD_ID"
      catchtable=", ISCATCHES"
      catchjoin="AND f.FISHSET_ID       = ISCATCHES.FISHSET_ID
                  AND f.SET_NO           = ISCATCHES.SET_NO"
      catchQ=paste0("AND ISCATCHES.SPECCD_ID IN (",caught,")")
    }else if (level.1=="By Gear"){
      focus="gear"
      the.gear<-get.gear()
      gear.GUI<-select.list(paste( the.gear$DESCRIPTION, " (", the.gear$GEARCD_ID,")",sep=""),
                            multiple=T, graphics=T, 
                            title="Choose a gear")
      gear<-SQL.in.noquotes(as.numeric(gsub('.+\\(([0-9]+)\\).*?$', '\\1', gear.GUI)))
      gearQ<-paste0("AND g.gearcd_id IN (",gear,")")
      
      name<-paste0('gear_',if(length(gear)>1) paste0(gear[1],"_etc") else gear[1])
    }
print("sought")
   print(sought) 
   print("caught")
   print(caught) 
   print("gear")
   print(gear) 
    
    # Choose Date Range -------------------------------------------------------
    #crashed my computer several times so limited to 3 years unless overridden
  verify.date.range<-function(date.range.GUI){
    diff <- as.Date(strptime(date.range.GUI[2], "%Y")) - as.Date(strptime(date.range.GUI[1], "%Y"))
    if (diff[[1]]>1095){
      verify.date.GUI<-select.list(c("Change my date range",
                                     "I might crash my computer, but I want more than 3 years of data"),
      multiple=F, graphics=T, 
      title="Please Verify your date selection")
      if (verify.date.GUI=="Change my date range")date.range.GUI=get.date.range()
    }
    return(date.range.GUI)
  }
  
  get.date.range<-function(sought, caught, gear){
    date.range.GUI<-list()
    date.range<-as.character(get.year(sought=sought, caught=caught, gear=gear))
    date.range.start<-select.list(date.range,
                                       multiple=F, graphics=T, 
                                       title="Choose the earliest year of desired data")
    date.range.end<-select.list(date.range,
                                        multiple=F, graphics=T, 
                                        title="Choose the most recent year of desired data")
    date.range.GUI<-list(date.range.start,date.range.end)
    date.range.GUI<-verify.date.range(date.range.GUI)
    return(date.range.GUI)
  }
  
  date.range<-get.date.range(sought=sought, caught=caught, gear=gear)

  
  dateQ<-paste0("AND t.board_date BETWEEN to_date('",date.range[1],"','YYYY') AND to_date('",date.range[2],"','YYYY')")

  tripcode=NULL
  setcode=NULL
    # Choose Set Code ---------------------------------------------------------   
    the.setcode<-get.setcode(tripcode=tripcode, sought=sought, date.range=date.range, gear=gear)
    setcode.GUI<-select.list(paste( the.setcode$SET_TYPE, " (", the.setcode$SETCD_ID,")",sep=""),
                             multiple=T, graphics=T, 
                             title="Choose a set type")
    if (length(setcode.GUI)<1){
    # if user cancels dialogue box, assume we want them all
      setcode<-SQL.in.noquotes(as.numeric(the.setcode[,1]))
    }else{
      setcode<-SQL.in.noquotes(as.numeric(gsub('.+\\(([0-9]+)\\).*?$', '\\1', setcode.GUI)))
    }
    setcodeQ<-paste0("AND f.setcd_id IN (",setcode,")")   
    # Choose Trip Code ---------------------------------------------------------    
    the.tripcode<-get.tripcode(setcode=setcode, sought=sought, date.range=date.range, gear=gear)
    tripcode.GUI<-select.list(paste( the.tripcode$TRIP_TYPE, " (", the.tripcode$TRIPCD_ID,")",sep=""),
                              multiple=T, graphics=T, 
                              title="Choose a trip type")
    if (length(setcode.GUI)<1){
      # if user cancels dialogue box, assume we want them all
      tripcode<-SQL.in.noquotes(as.numeric(tripcode.GUI[,1]))      
    }else{
      tripcode<-SQL.in.noquotes(as.numeric(gsub('.+\\(([0-9]+)\\).*?$', '\\1', tripcode.GUI)))
    }
    
    tripcodeQ<-paste0("AND t.tripcd_id IN (",tripcode,")")
    


    
    where<-paste(soughtQ,
                 gearQ,
                 setcodeQ,
                 tripcodeQ,
                 dateQ,
                 catchQ,sep=" ")

    
 # a bad month somwhere is causing the extraction to fail in certain circumstances.  Must investigate   
 #   to_date(NVL(to_char( p1.setdate, 'YYYY-MM-DD'),'0000-00-00') ||' '|| SUBSTR( p1.settime,1,2) || ':' || substr( p1.settime, 3, 2),'YYYY-MM-DD HH24:MI') p1time,
#    to_date(NVL(to_char( p2.setdate, 'YYYY-MM-DD'),'0000-00-00') ||' '|| SUBSTR( p2.settime,1,2) || ':' || substr( p2.settime, 3, 2),'YYYY-MM-DD HH24:MI') p2time,
#    to_date(NVL(to_char( p3.setdate, 'YYYY-MM-DD'),'0000-00-00') ||' '|| SUBSTR( p3.settime,1,2) || ':' || substr( p3.settime, 3, 2),'YYYY-MM-DD HH24:MI') p3time,
#    to_date(NVL(to_char( p4.setdate, 'YYYY-MM-DD'),'0000-00-00') ||' '|| SUBSTR( p4.settime,1,2) || ':' || substr( p4.settime, 3, 2),'YYYY-MM-DD HH24:MI') p4time,
    
  ISD_INF_query<-paste0("SELECT SUBSTR(v.vessel_name,1,15) vessel,
                        t.tripcd_id,
                        to_char(t.board_date,'YYYY') year,
                        t.board_date,
                        v.cfv,
                        t.trip,
                        f.set_no,
                        f.nafarea_id,
                        f.stratum_id,
                        to_number(f.station) station,
                        NVL(f.haulccd_id,1) haulccd_id,
                        p1.longitude*-1 p1long,
                        p1.latitude p1lat,
                        p2.longitude*-1 p2long,
                        p2.latitude p2lat,
                        p3.longitude*-1 p3long,
                        p3.latitude p3lat,
                        p4.longitude*-1 p4long,
                        p4.latitude p4lat,
                        p1.longddmm p1longddmm,
                        p1.latddmm p1latddmm,
                        p2.longddmm p2longddmm,
                        p2.latddmm p2latddmm,
                        p3.longddmm p3longddmm,
                        p3.latddmm p3latddmm,
                        p4.longddmm p4longddmm,
                        p4.latddmm p4latddmm,
                        p4.settime p4time,len_longline lenLLkm,
                        ROUND((f.num_hook_haul/ 1000.), 2) nK_hooks,
                        p1.depth p1depth,
                        p2.depth p2depth,
                        p3.depth p3depth,
                        p4.depth p4depth,
                        g.gearcd_id,
                        g.hookcd_id,
                        g.hooksize,
                        f.num_hook_haul,
                        f.specscd_id,
                        t.vess_id,
                        t.trip_id,
                        f.fishset_id,
                        f.setcd_id,
                        a.set_type,
                        p1.setprof_id p1setprof_id,
                        p4.setprof_id p4setprof_id
                        ",catchfield,"
                        FROM observer.isTrips t,
                        observer.isvessels v,
                        observer.isgears g,
                        (SELECT f.fishset_id f_id,
                        p.*
                        FROM
                        (SELECT * FROM observer.issetprofile WHERE pntcd_id=1
                        ) p,
                        observer.isFishSets f
                        WHERE p.fishset_id(+)=f.fishset_id
                        ) p1,
                        (SELECT f.fishset_id f_id,
                        p.*
                        FROM
                        (SELECT * FROM observer.issetprofile WHERE pntcd_id=2
                        ) p,
                        observer.isFishSets f
                        WHERE p.fishset_id(+)=f.fishset_id
                        ) p2,
                        (SELECT f.fishset_id f_id,
                        p.*
                        FROM
                        (SELECT * FROM observer.issetprofile WHERE pntcd_id=3
                        ) p,
                        observer.isFishSets f
                        WHERE p.fishset_id(+)=f.fishset_id
                        ) p3,
                        (SELECT f.fishset_id f_id,
                        p.*
                        FROM
                        (SELECT * FROM observer.issetprofile WHERE pntcd_id=4
                        ) p,
                        observer.isFishSets f
                        WHERE p.fishset_id(+)=f.fishset_id
                        ) p4,
                        observer.isFishSets f,
                        observer.issettypecodes a
                        ", catchtable,"
                        WHERE f.fishset_id=p1.f_id
                        AND f.fishset_id  =p2.f_id
                        AND f.fishset_id  =p3.f_id
                        AND f.fishset_id  =p4.f_id
                        AND t.vess_id     =v.vess_id
                        AND t.trip_id     =f.trip_id
                        AND g.gear_id     =f.gear_id
                        AND f.setcd_id    = a.setcd_id
                        ", catchjoin,"
                        ",where)
  if(focus=="spp")print("Looking for your species...")else print("Looking for your gear...")
  print("###")
  print(ISD_INF_query)
  print("###")
  ISD_INF<<-sqlQuery(channel,ISD_INF_query)
  if (nrow(ISD_INF)<1){
    print("Error: No data can be found for your selection")
    print("####")
    print(ISD_INF_query)
    print("####")
    return(NULL)
  }else{
    print("Found data. Transmogrifying universes...")
    p1.dat<-sqldf("SELECT FISHSET_ID, 1 ORD, P1LONG X, P1LAT Y FROM ISD_INF")
    p1.dat<-p1.dat[complete.cases(p1.dat),]
    p2.dat<-sqldf("SELECT FISHSET_ID, 2 ORD, P2LONG X, P2LAT Y FROM ISD_INF")
    p2.dat<-p2.dat[complete.cases(p2.dat),]
    p3.dat<-sqldf("SELECT FISHSET_ID, 3 ORD, P3LONG X, P3LAT Y FROM ISD_INF")
    p3.dat<-p3.dat[complete.cases(p3.dat),]
    p4.dat<-sqldf("SELECT FISHSET_ID, 4 ORD, P4LONG X, P4LAT Y FROM ISD_INF")
    p4.dat<-p4.dat[complete.cases(p4.dat),]
    
    #identify the sets with at least 2 points
    #order them in seq;  
    #capture the unique fishset_id
    
    #combine the various combinations of points to get potential tracks
    #p1-p2:set.deploy
    #p2-p3:set.tow
    #p3-p4:set.retrieve
    #p1-p4:set.all

    set.deploy<-rbind(p1.dat,p2.dat)
    set.deploy.segs<-set.deploy[duplicated(set.deploy$FISHSET_ID),]$FISHSET_ID
    set.deploy<-set.deploy[set.deploy$FISHSET_ID %in% set.deploy.segs,]
    set.deploy<-set.deploy[order(set.deploy$FISHSET_ID,set.deploy$ORD),]
    set.deploy.unq<-unique(set.deploy$FISHSET_ID)
    
    set.tow<-rbind(p2.dat,p3.dat)
    set.tow.segs<-set.tow[duplicated(set.tow$FISHSET_ID),]$FISHSET_ID
    set.tow<-set.tow[set.tow$FISHSET_ID %in% set.tow.segs,]
    set.tow<-set.tow[order(set.tow$FISHSET_ID,set.tow$ORD),]
    set.tow.unq<-unique(set.tow$FISHSET_ID)
    
    set.retrieve<-rbind(p3.dat,p4.dat)
    set.retrieve.segs<-set.retrieve[duplicated(set.retrieve$FISHSET_ID),]$FISHSET_ID
    set.retrieve<-set.retrieve[set.retrieve$FISHSET_ID %in% set.retrieve.segs,]
    set.retrieve<-set.retrieve[order(set.retrieve$FISHSET_ID,set.retrieve$ORD),]
    set.retrieve.unq<-unique(set.retrieve$FISHSET_ID)
    
    set.all<-rbind(p1.dat,p2.dat,p3.dat,p4.dat)
    set.all.segs<-set.all[duplicated(set.all$FISHSET_ID),]$FISHSET_ID
    set.all<-set.all[set.all$FISHSET_ID %in% set.all.segs,]
    set.all<-set.all[order(set.all$FISHSET_ID,set.all$ORD),]
    set.all.unq<-unique(set.all$FISHSET_ID)
    
    bad.sets<-set.deploy[!duplicated(set.deploy$FISHSET_ID),]$FISHSET_ID
    bad.sets<-c(bad.sets,set.tow[!duplicated(set.tow$FISHSET_ID),]$FISHSET_ID)
    bad.sets<-c(bad.sets,set.retrieve[!duplicated(set.retrieve$FISHSET_ID),]$FISHSET_ID)
    
    #The following gets set attributes for use in the data layer
    #It will be fancy when it converts p1-p4 to datetime fields so that soaktime and duration can be checked
    # and can handle the ocassional null value for edatetime
    
    #p1time,
    #p2time,
    #p3time,
    #p4time,
    
#     year,
#     tripcd_id,
#     cfv,
#     vessel,
#     trip,
#     trip_id,
#     set_no,
#     nafarea_id,
#     stratum_id,
#     gearcd_id,
#     specscd_id,
#     FISHSET_ID,
#     p1longddmm,
#     p2longddmm,
#     p3longddmm,
#     p4longddmm,
#     p1latddmm,
#     p2latddmm,
#     p3latddmm,
#     p4latddmm
    set.all.attrib<-sqldf("SELECT * FROM ISD_INF")
    
    rownames(set.all.attrib)<-set.all.attrib$FISHSET_ID
    
    set.deploy<-merge(set.deploy,set.all.attrib)
    set.tow<- merge(set.tow,set.all.attrib)
    set.retrieve<-merge(set.retrieve,set.all.attrib)
    set.all<-merge(set.all,set.all.attrib)
    bad.sets<-merge(bad.sets,set.all.attrib)
  
    results=list(set.deploy,set.tow,set.retrieve,set.all,bad.sets)
  }
  print(paste("Completed"))
  return(results)
}
# load required ecomod functions
# loadfunctions("observers/src/_RFunctions")
# loadfunctions("utility/src/_Rfunctions/datetime")
# loadfunctions("utility/src/_Rfunctions/sql.tools")
# #connect to db
# library(RODBC)
# channel<-odbcConnect("PTRAN",uid=oracle.observer.username,pwd=oracle.observer.password)
# #run it
# test<-observer.track.selector()
