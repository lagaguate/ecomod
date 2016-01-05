#//Written to help QC Halibut data but might be broadly applicable to ISDB
#//Extracts stations and generates shapefile

library(RODBC)
library(sp) #coordinates; crs; proj4string
library(rgdal)
channel<-odbcConnect("PTRAN",uid=oracle.halibut.username,pwd=oracle.halibut.password)
#Generate Station Data
stations.shp.query=paste0("Select STATION, LON*-1 LON, LAT, NAFAREA, STRATUM,STA_ID, COMMENTS from isdb_halibut.planned_locations")
stations.shp<-sqlQuery(channel,stations.shp.query)
stations.shp<-stations.shp[!is.na(stations.shp$LON)|!is.na(stations.shp$LAT),]