#//Written to help QC Halibut data but might be broadly applicable to ISDB
#//Extracts P1-P4 stations and generates shapefiles
library(RODBC)
library(sp) #coordinates; crs; proj4string
library(rgdal)
channel<-odbcConnect("PTRAN",uid=oracle.halibut.username,pwd=oracle.halibut.password)


#Generate Set Data - get positions ("set.all") and attributes seperately,
#join on fishset_id
p1.query=paste0("SELECT FISHSET_ID, 1 ord, P1LONG X, P1LAT Y FROM isdb_halibut.ISD_INF")
p1.dat<-sqlQuery(channel,p1.query)
p1.dat<-p1.dat[complete.cases(p1.dat),]
p2.query=paste0("SELECT FISHSET_ID, 2 ord, P2LONG X, P2LAT Y FROM isdb_halibut.ISD_INF")
p2.dat<-sqlQuery(channel,p2.query)
p2.dat<-p2.dat[complete.cases(p2.dat),]
p3.query=paste0("SELECT FISHSET_ID, 3 ord, P3LONG X, P3LAT Y FROM isdb_halibut.ISD_INF")
p3.dat<-sqlQuery(channel,p3.query)
p3.dat<-p3.dat[complete.cases(p3.dat),]
p4.query=paste0("SELECT FISHSET_ID, 4 ord, P4LONG X, P4LAT Y FROM isdb_halibut.ISD_INF")
p4.dat<-sqlQuery(channel,p4.query)
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
set.all.attrib.query<-paste0("SELECT FISHSET_ID,TRIP,TRIPCD_ID,
                             YEAR,
                             VESSEL,
                             CFV,
                             SET_NO,
                             STATION,
                             nvl(to_char(SDAYTIME, 'YYYY-MM-DD HH24:MI:SS'), '0000-00-00 00:00:00') P1TIME,
                             /*
                             CASE
                             WHEN to_date(NVL(to_char(SDAYTIME, 'YYYY-MM-DD'),'0000-00-00') ||' '|| SUBSTR(P2TIME,1,2) || ':' || substr(P2TIME, 3, 2),'YYYY-MM-DD HH24:MI') - to_date(NVL(to_char(SDAYTIME, 'YYYY-MM-DD HH24:MI:SS'),'0000-00-00 00:00:00'),'YYYY-MM-DD HH24:MI:SS') >0
                             THEN to_date(NVL(to_char(SDAYTIME, 'YYYY-MM-DD'),'0000-00-00') ||' '|| SUBSTR(P2TIME,1,2) || ':' || substr(P2TIME, 3, 2),'YYYY-MM-DD HH24:MI')
                             ELSE to_date(NVL(to_char(EDAYTIME, 'YYYY-MM-DD'),'0000-00-00') ||' '|| SUBSTR(P2TIME,1,2) || ':' || substr(P2TIME, 3, 2),'YYYY-MM-DD HH24:MI') 
                             END AS P2TIME,
                             CASE
                             WHEN to_date(NVL(to_char(EDAYTIME, 'YYYY-MM-DD HH24:MI:SS'),'0000-00-00 00:00:00'),'YYYY-MM-DD HH24:MI:SS') - to_date(NVL(to_char(SDAYTIME,'YYYY-MM-DD'),'YYYY-MM-DD') ||' '|| SUBSTR(P3TIME,1,2) || ':' || substr(P3TIME, 3, 2),'YYYY-MM-DD HH24:MI')>1
                             THEN to_date(NVL(to_char(EDAYTIME,'YYYY-MM-DD'),'0000-00-00') ||' '|| SUBSTR(P3TIME,1,2) || ':' || substr(P3TIME, 3, 2),'YYYY-MM-DD HH24:MI')
                             ELSE to_date(NVL(to_char(SDAYTIME,'YYYY-MM-DD'),'0000-00-00') ||' '|| SUBSTR(P3TIME,1,2) || ':' || substr(P3TIME, 3, 2),'YYYY-MM-DD HH24:MI') 
                             END AS P3TIME,
                             */
                             nvl(to_char(EDAYTIME, 'YYYY-MM-DD HH24:MI:SS'), '0000-00-00 00:00:00') P4TIME,
                             LENLLKM,
                             NK_HOOKS,
                             GEAR_LEN_M,
                             GEARCD_ID,
                             HOOKCD_ID,
                             HOOKSIZE,
                             NUM_HOOK_HAUL,
                             HOOKSPACINGM,
                             DURATION,
                             SOAKMINP3P2
                             FROM isdb_halibut.ISD_INF")
set.all.attrib<-sqlQuery(channel,set.all.attrib.query)

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
#generate the line objects (use fishset_id as ID)
set.all.shp<-SpatialLinesDataFrame(SpatialLines(set.all.lines), set.all.attrib, match.ID = T)
set.deploy.shp<-SpatialLinesDataFrame(SpatialLines(set.deploy.lines), set.all.attrib[set.all.attrib$FISHSET_ID %in% set.deploy.unq, ], match.ID = T)
set.tow.shp<-SpatialLinesDataFrame(SpatialLines(set.tow.lines),       set.all.attrib[set.all.attrib$FISHSET_ID %in% set.tow.unq, ], match.ID = T)
set.retrieve.shp<-SpatialLinesDataFrame(SpatialLines(set.retrieve.lines), set.all.attrib[set.all.attrib$FISHSET_ID %in% set.retrieve.unq, ], match.ID = T)

#set the projection
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
proj4string(set.all.shp) <- crs.geo
proj4string(set.tow.shp) <- crs.geo
proj4string(set.deploy.shp) <- crs.geo
proj4string(set.retrieve.shp) <- crs.geo

#write the files
writeOGR(set.all.shp, ".", "Halibut_Sets_All", driver="ESRI Shapefile",overwrite_layer=T)
writeOGR(set.deploy.shp, ".", "Halibut_Sets_Deploy", driver="ESRI Shapefile",overwrite_layer=T)
writeOGR(set.tow.shp, ".", "Halibut_Sets_Tows", driver="ESRI Shapefile",overwrite_layer=T)
writeOGR(set.retrieve.shp, ".", "Halibut_Sets_Retrieve", driver="ESRI Shapefile",overwrite_layer=T)
