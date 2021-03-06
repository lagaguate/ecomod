oceans.get.data<-function(dsn, user, pw, datawindow, last_n_days, startDate, endDate, vessel_list){
  options(stringsAsFactors=F)
  require(RODBC)
  channel<-connectAs(dsn, user,pw)
  #stole SQL from VDC.MWQUERY.DATA (ID=5255)
  #The following SQL was pulled from the VDC and it's one of the Oceans activity area queries
  if (is.null(datawindow) || datawindow == "All" ){
    where1= ""
  }else{
    where1= paste("WHERE windID = '", datawindow, "'", sep="")
  }
  
  where2<-NULL
  #if a date range has been provided, use it, otherwise, get last_n_days
  if (!is.null(startDate) && !is.null(endDate)){
    where2= paste("v.pos_date_local BETWEEN To_Date('",startDate,"','YYYY-MM-DD HH24:MI') AND To_Date('",endDate,"','YYYY-MM-DD HH24:MI')",sep="")
    title= paste("Activity in ", datawindow, " between ",startDate," and ",endDate,sep="")
  }else if (!is.null(last_n_days) && (!is.null(startDate) || !is.null(endDate))){
    thedate<-c(startDate,endDate)
    where2= paste("v.pos_date_local BETWEEN (To_Date('",thedate,"','YYYY-MM-DD HH24:MI') - ",as.numeric(last_n_days),") AND (To_Date('",thedate,"','YYYY-MM-DD HH24:MI'))",sep="")
    title= paste(last_n_days," days of activity prior to ", thedate," in ",datawindow,sep="")
  }else if (!is.null(last_n_days)){
    where2= paste("v.pos_date_local >= sysdate - '",as.numeric(last_n_days),"'",sep="")
    title= paste("Last ",last_n_days," days of activity in ",datawindow,sep="")
  }else{
    where2= paste("v.pos_date_local >= sysdate - ",last_n_days,sep="")
    title= paste("Last ",last_n_days," days of activity in ",datawindow,sep="")
  }
    
  #if a vessel_list has been provided, add it to the SQL
   if (!is.null(vessel_list)&&vessel_list!='NA'){
      #thevrn<-paste(vessel_list,collapse="', '")
    thevrn<-paste("'",gsub("(\\s*,*\\s,*\\s*)","','",vessel_list),"'",sep="")
    #thevrntitle<-thevrn
    where2= paste(where2, " AND vr_number IN (",thevrn,")",sep="")
    title<-paste(title, " for VRN: ",thevrn,sep="")
  }else{
    thevrn<-NULL
    thevrntitle<-NULL
  }
  
  the.SQL <-paste("
    SELECT
    source,
    position_utc,
    position_local,
    LAT,
    LON,
    activity,
    vr_number,
    VCLASS,
    vessel_name,
    LICENCE_ID,
    hailstatus,
    COMMUNITY,
    DETACHMENT,
    GEAR,
    vessel_name ||'-'|| v.position_local||'-'||log_efrt_std_info_id AS setid,
    vessel_name||':'||trip_id vesselTrip,
    trip_id,
    log_efrt_std_info_id subtrip,
    HAIL_OUT_SPECIES,
    HAIL_OUT_SPECIES_CATEGORY,
    SPECIES,
    SPECIES_CATEGORY,
    RND_WEIGHT_kgs,
    RPT_WEIGHT_kgs,
    VAL_CDN$
    FROM ocmd_mpa.vessel_events_kml v ,
    (SELECT MIN(minLat) minLat,
    MIN(minLon) minLon,
    MAX(maxLat) maxLat,
    MAX(maxLon) maxLon
    FROM ocmd_mpa.data_window
    ", where1, " ) w
    WHERE v.lat BETWEEN w.minLat AND w.maxLat
    AND v.lon BETWEEN w.minLon AND w.maxLon  
    AND ", where2 ,sep="")
	
  vertexFields = c("LON","LAT","POSITION_UTC")
  cat("Getting Vessel data\n")
  #cat(the.SQL)
  df<-sqlQuery(channel,the.SQL)
  #CATEGORIZE THE HAIL_OUT SPECIES INTO GROUPS - THESE GROUPS WILL SUBDIVIDE GEAR TYPES
  pelagics<-c("ALEWIVES/GASPEREAU",
              "ALEWIVES/GASPEREAU-HERRING/MACKEREL",
              "BILLFISH",
              "EEL",
              "EEL-HERRING",
              "HERRING",
              "HERRING/MACKEREL",
              "HERRING-MACKEREL",
              "HERRING-HERRING/MACKEREL",
              "MACKEREL",
              "SHARK",
              "SHARK-SWORDFISH",
              "STURGEON",
              "SWORDFISH",
              "SWORDFISH-TUNA",
              "SWORDFISH-TUNA, RESTRICTED",
              "TUNA",
              "TUNA, BIGEYE",
              "TUNA, BLUEFIN",
              "TUNA, RESTRICTED",
              "TUNA-TUNA, RESTRICTED"
              )
  
  shellfish<-c("CLAMS, DEPURATED",
               "CLAMS, STIMPSON SURF",
               "CLAMS-OYSTERS, AMERICAN",
               "CRAB, GREEN",
               "CRAB, JONAH",
               "CRAB, JONAH-LOBSTER",
               "CRAB, RED",
               "CRAB, ROCK",
               "CRAB, SNOW",
               "CRAB, SPIDER/TOAD",
               "CRAB, STONE",
               "JONAH/ROCK",
               "LOBSTER",
               "LOBSTER - GREY ZONE",
               "MOLLUSCS",
               "OCEAN QUAHAUG",
               "OYSTERS, AMERICAN",  
               "SCALLOP, SEA",
               "SCALLOP, SEA-SEA URCHINS",
               "SEA CUCUMBER",
               "SEA URCHINS",
               "SHRIMP, PANDALUS BOREALIS",
               "SQUID",
               "WHELKS")
  
  groundfish<-c("AMERICAN PLAICE",
                "COD",
                "CUSK",
                "GREYSOLE/WITCH",
                "GROUNDFISH",
                "HAGFISH (SLIME EEL)",
                "HADDOCK",
                "HALIBUT",
                "REDFISH",
                "POLLOCK",
                "SILVER HAKE",
                "WHITE HAKE"
                )
  
  other<-c("MARINE PLANT",
          "SEAL",
          "SEAL SKINS/HARP/RAG.JACKET (NO.)"
          )
  
  mysterycategory<-c("CRAB, JONAH-GROUNDFISH",
                 "CRAB, JONAH-SWORDFISH", 
                 "CRAB, JONAH-LOBSTER",
                 "CRAB, ROCK-GROUNDFISH",
                 "CRAB, SNOW-HERRING",
                 "CRAB, SNOW-MACKEREL",
                 "CRAB, SNOW-SCALLOP, SEA",
                 "CRAB, SNOW-SWORDFISH",
                 "GROUNDFISH-HERRING",
                 "GROUNDFISH-SCALLOP, SEA",
                 "GROUNDFISH-SWORDFISH",
                 "GROUNDFISH-TUNA",
                 "HERRING-SCALLOP, SEA",
                 "ITEMS",
                 "ITEMS-SCALLOP, SEA",
                 "LOBSTER-SWORDFISH",
                 "SCALLOP, SEA-SHARK",
                ".na."
                 )
  
  df$HAIL_OUT_SPECIES_CATEGORY[df$HAIL_OUT_SPECIES %in% pelagics] <- "PELAGIC"    
  df$HAIL_OUT_SPECIES_CATEGORY[df$HAIL_OUT_SPECIES %in% shellfish] <- "SHELLFISH"  
  df$HAIL_OUT_SPECIES_CATEGORY[df$HAIL_OUT_SPECIES %in% groundfish] <- "GROUNDFISH"  
  df$HAIL_OUT_SPECIES_CATEGORY[df$HAIL_OUT_SPECIES %in% other] <- "OTHER"
  df$HAIL_OUT_SPECIES_CATEGORY[df$HAIL_OUT_SPECIES %in% mysterycategory] <- "Unknown"

  the.SQL.dwindow<-paste("SELECT MIN(minLat) minLat,
                         MIN(minLon) minLon,
                         MAX(maxLat) maxLat,
                         MAX(maxLon) maxLon
                         FROM ocmd_mpa.data_window
                         ", where1,sep="")

  dWindow<-sqlQuery(channel,the.SQL.dwindow)

  df<-list(df[with(df, order(VR_NUMBER, POSITION_UTC)), ],vertexFields, the.SQL,last_n_days, startDate, endDate, thevrn, datawindow, dWindow, title)
  dfKeep<<-df
  odbcClose(channel)
  return(df)
}