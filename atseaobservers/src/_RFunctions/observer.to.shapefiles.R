#//Originally written to help QC Halibut data but can be more broadly applied to 
#//all ISDB data
#//Extracts P1-P4 stations and generates shapefiles
observer.to.shapefiles<-function(GUI=T,sought=11){
library(RODBC)
library(sp) #coordinates; crs; proj4string
library(rgdal)
library(sqldf)
channel<-odbcConnect("PTRAN",uid=oracle.observer.username,pwd=oracle.observer.password)
sought=6600
ddmm<-201201
if (GUI ==F){
  sought=sought
  } else {
  get.species<-function(){
                  species.query="SELECT DISTINCT COMMON, SPECSCD_ID
                  FROM SPECIESSOUGHTCODES
                  ORDER BY COMMON"
                  the.species = sqlQuery(channel, species.query)
                  return(the.species)
  }
  the.species<-get.species()
  sought.GUI<-select.list(paste( the.species$COMMON, " (", the.species$SPECSCD_ID,")",sep=""),
                           multiple=F, graphics=T, 
                           title="Choose a species")
sought<-as.numeric(gsub('.+\\(([0-9]+)\\).*?$', '\\1', sought.GUI))
}
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
  p1.setdate p1date,
  p1.settime p1time,
  p2.setdate p2date,
  p2.settime p2time,
  p3.setdate p3date,
  p3.settime p3time,
  p4.setdate p4date,
  p4.settime p4time,
  len_longline lenLLkm,
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
  WHERE f.fishset_id=p1.f_id
  AND f.fishset_id  =p2.f_id
  AND f.fishset_id  =p3.f_id
  AND f.fishset_id  =p4.f_id
  AND t.vess_id     =v.vess_id
  AND t.trip_id     =f.trip_id
  AND g.gear_id     =f.gear_id
  AND f.setcd_id    = a.setcd_id
  AND f.specscd_id IN (",sought,")
  AND t.board_date > to_date('",ddmm,"','YYYYMM')")

ISD_INF<-sqlQuery(channel,ISD_INF_query)

p1.dat<-sqldf("SELECT FISHSET_ID, 1 ORD, P1LONG X, P1LAT Y FROM ISD_INF")
p1.dat<-p1.dat[complete.cases(p1.dat),]
p2.dat<-sqldf("SELECT FISHSET_ID, 2 ORD, P2LONG X, P2LAT Y FROM ISD_INF")
p2.dat<-p2.dat[complete.cases(p2.dat),]
p3.dat<-sqldf("SELECT FISHSET_ID, 3 ORD, P3LONG X, P3LAT Y FROM ISD_INF")
p3.dat<-p3.dat[complete.cases(p3.dat),]
p4.dat<-sqldf("SELECT FISHSET_ID, 4 ORD, P4LONG X, P4LAT Y FROM ISD_INF")
p4.dat<-p4.dat[complete.cases(p4.dat),]

#combine the various combinations of useful points
#identify the sets with at least 2 points
#order them in seq;  
#capture the unique fishset_id
set.deploy<-rbind(p1.dat,p2.dat)
set.deploy.segs<-set.deploy[duplicated(set.deploy$FISHSET_ID),]$FISHSET_ID
set.deploy<-set.deploy[set.deploy$FISHSET_ID %in% set.deploy.segs,]
set.deploy<-set.deploy[order(set.deploy$FISHSET_ID,set.deploy$ORD),]
set.deploy.unq<-unique(set.deploy$FISHSET_ID)

bad.sets<-set.deploy[!duplicated(set.deploy$FISHSET_ID),]$FISHSET_ID

set.tow<-rbind(p2.dat,p3.dat)
set.tow.segs<-set.tow[duplicated(set.tow$FISHSET_ID),]$FISHSET_ID
set.tow<-set.tow[set.tow$FISHSET_ID %in% set.tow.segs,]
set.tow<-set.tow[order(set.tow$FISHSET_ID,set.tow$ORD),]
set.tow.unq<-unique(set.tow$FISHSET_ID)

bad.sets<-c(bad.sets,set.tow[!duplicated(set.tow$FISHSET_ID),]$FISHSET_ID)

set.retrieve<-rbind(p3.dat,p4.dat)
set.retrieve.segs<-set.retrieve[duplicated(set.retrieve$FISHSET_ID),]$FISHSET_ID
set.retrieve<-set.retrieve[set.retrieve$FISHSET_ID %in% set.retrieve.segs,]
set.retrieve<-set.retrieve[order(set.retrieve$FISHSET_ID,set.retrieve$ORD),]
set.retrieve.unq<-unique(set.retrieve$FISHSET_ID)

bad.sets<-c(bad.sets,set.retrieve[!duplicated(set.retrieve$FISHSET_ID),]$FISHSET_ID)

set.all<-rbind(p1.dat,p2.dat,p3.dat,p4.dat)
set.all.segs<-set.all[duplicated(set.all$FISHSET_ID),]$FISHSET_ID
set.all<-set.all[set.all$FISHSET_ID %in% set.all.segs,]
set.all<-set.all[order(set.all$FISHSET_ID,set.all$ORD),]
set.all.unq<-unique(set.all$FISHSET_ID)

#The following gets set attributes for use in the data layer
#It will be fancy when it converts p1-p4 to datetime fields so that soaktime and duration can be checked
# and can handle the ocassional null value for edatetime
set.all.attrib<-sqldf("SELECT year,
FISHSET_ID,
gearcd_id,
                             trip,

                             set_no,
                             nafarea_id,
                             stratum_id  FROM ISD_INF")
# set.all.attrib.query<-paste0("SELECT FISHSET_ID,TRIP,TRIPCD_ID,
#                              YEAR,
#                              VESSEL,
#                              CFV,
#                              SET_NO,
#                              STATION,
#                              nvl(to_char(SDAYTIME, 'YYYY-MM-DD HH24:MI:SS'), '0000-00-00 00:00:00') P1TIME,
#                              /*
#                              CASE
#                              WHEN to_date(NVL(to_char(SDAYTIME, 'YYYY-MM-DD'),'0000-00-00') ||' '|| SUBSTR(P2TIME,1,2) || ':' || substr(P2TIME, 3, 2),'YYYY-MM-DD HH24:MI') - to_date(NVL(to_char(SDAYTIME, 'YYYY-MM-DD HH24:MI:SS'),'0000-00-00 00:00:00'),'YYYY-MM-DD HH24:MI:SS') >0
#                              THEN to_date(NVL(to_char(SDAYTIME, 'YYYY-MM-DD'),'0000-00-00') ||' '|| SUBSTR(P2TIME,1,2) || ':' || substr(P2TIME, 3, 2),'YYYY-MM-DD HH24:MI')
#                              ELSE to_date(NVL(to_char(EDAYTIME, 'YYYY-MM-DD'),'0000-00-00') ||' '|| SUBSTR(P2TIME,1,2) || ':' || substr(P2TIME, 3, 2),'YYYY-MM-DD HH24:MI') 
#                              END AS P2TIME,
#                              CASE
#                              WHEN to_date(NVL(to_char(EDAYTIME, 'YYYY-MM-DD HH24:MI:SS'),'0000-00-00 00:00:00'),'YYYY-MM-DD HH24:MI:SS') - to_date(NVL(to_char(SDAYTIME,'YYYY-MM-DD'),'YYYY-MM-DD') ||' '|| SUBSTR(P3TIME,1,2) || ':' || substr(P3TIME, 3, 2),'YYYY-MM-DD HH24:MI')>1
#                              THEN to_date(NVL(to_char(EDAYTIME,'YYYY-MM-DD'),'0000-00-00') ||' '|| SUBSTR(P3TIME,1,2) || ':' || substr(P3TIME, 3, 2),'YYYY-MM-DD HH24:MI')
#                              ELSE to_date(NVL(to_char(SDAYTIME,'YYYY-MM-DD'),'0000-00-00') ||' '|| SUBSTR(P3TIME,1,2) || ':' || substr(P3TIME, 3, 2),'YYYY-MM-DD HH24:MI') 
#                              END AS P3TIME,
#                              */
#                              nvl(to_char(EDAYTIME, 'YYYY-MM-DD HH24:MI:SS'), '0000-00-00 00:00:00') P4TIME,
#                              LENLLKM,
#                              NK_HOOKS,
#                              GEAR_LEN_M,
#                              GEARCD_ID,
#                              HOOKCD_ID,
#                              HOOKSIZE,
#                              NUM_HOOK_HAUL,
#                              HOOKSPACINGM,
#                              DURATION,
#                              SOAKMINP3P2
#                              FROM isdb_halibut.ISD_INF")
# set.all.attrib<-sqlQuery(channel,set.all.attrib.query)

rownames(set.all.attrib)<-set.all.attrib$FISHSET_ID

#generate the line objects (use fishset_id as ID)
set.all.lines<-list()
set.deploy.lines<-list()
set.tow.lines<-list()
set.retrieve.lines<-list()
d.cnt<-1
t.cnt<-1
r.cnt<-1
for (i in 1:length(set.all.unq)){
  this.set <-set.all.unq[i]
  set.all.lines[i]<-Lines(Line(set.all[set.all$FISHSET_ID==this.set,][3:4]),ID=this.set)
  if (this.set %in% set.deploy.unq){
    set.deploy.lines[d.cnt]<-Lines(Line(set.deploy[set.deploy$FISHSET_ID==this.set,][3:4]),ID=this.set)
    d.cnt<-d.cnt+1
  }
  if (this.set %in% set.tow.unq){
    set.tow.lines[t.cnt]<-Lines(Line(set.tow[set.tow$FISHSET_ID==this.set,][3:4]),ID=this.set)
    t.cnt<-t.cnt+1
  }  
  if (this.set %in% set.retrieve.unq){
    set.retrieve.lines[r.cnt]<-Lines(Line(set.retrieve[set.retrieve$FISHSET_ID==this.set,][3:4]),ID=this.set)
    r.cnt<-r.cnt+1
  }
}

#set the projection
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
#generate the line objects (use fishset_id as ID)
set.all.shp<-SpatialLinesDataFrame(SpatialLines(set.all.lines), set.all.attrib, match.ID = T)
proj4string(set.all.shp) <- crs.geo
writeOGR(set.all.shp, dsn=project.datadirectory("atseaobservers"),  paste0("Sets_All_",sought,"_",ddmm), driver="ESRI Shapefile",overwrite_layer=T)

if (d.cnt>1){
  set.deploy.shp<-SpatialLinesDataFrame(SpatialLines(set.deploy.lines), set.all.attrib[set.all.attrib$FISHSET_ID %in% set.deploy.unq, ], match.ID = T)
  proj4string(set.deploy.shp) <- crs.geo
  writeOGR(set.deploy.shp, dsn=project.datadirectory("atseaobservers"),  paste0("Sets_Deploy_",sought,"_",ddmm), driver="ESRI Shapefile",overwrite_layer=T)
}
if (t.cnt>1){
  set.tow.shp<-SpatialLinesDataFrame(SpatialLines(set.tow.lines),       set.all.attrib[set.all.attrib$FISHSET_ID %in% set.tow.unq, ], match.ID = T)
  proj4string(set.tow.shp) <- crs.geo
  writeOGR(set.tow.shp, dsn=project.datadirectory("atseaobservers"),  paste0("Sets_Tow_",sought,"_",ddmm), driver="ESRI Shapefile",overwrite_layer=T)
}
if (r.cnt>1){
  set.retrieve.shp<-SpatialLinesDataFrame(SpatialLines(set.retrieve.lines), set.all.attrib[set.all.attrib$FISHSET_ID %in% set.retrieve.unq, ], match.ID = T)
  proj4string(set.retrieve.shp) <- crs.geo
  writeOGR(set.retrieve.shp, dsn=project.datadirectory("atseaobservers"),  paste0("Sets_Retrieve_",sought,"_",ddmm), driver="ESRI Shapefile",overwrite_layer=T)
}
}
