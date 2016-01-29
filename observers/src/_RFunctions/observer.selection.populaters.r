populate.species <- function(order = "COMMON", setcode = NULL,tripcode = NULL, date.range = NULL, sought = NULL, caught = NULL, gear = NULL, vessels = NULL, coords = NULL) {
  if (order != "COMMON")  order = "SPECSCD_ID"
  if (is.null(setcode) & is.null(tripcode) & is.null(date.range) & is.null(sought) & is.null(caught) & is.null(gear) & is.null(vessels) & is.null(coords)){
  species.query = paste0("SELECT DISTINCT COMMON SOUGHT, SPECSCD_ID
                         FROM OBSERVER.SPECIESSOUGHTCODES
                         ORDER BY ",order)
  }else{
    settweak=""
    triptweak=""
    datetweak=""
    soughttweak=""
    caughttweak=""
    geartweak=""
    vesseltweak=""
    coordstweak=""
    
    fishsetjoinALL=""
    tripjoinALL=""
    setjoin=""
    tripjoin=""
    datejoin=""
    soughtjoin=""
    caughtjoin=""
    gearjoin=""
    vesseljoin=""
    coordsjoin=""
    
    ######
    #FISHSETSJOIN - needed by set, trip, date, gear,vessels
    if (!is.null(setcode) | !is.null(tripcode) | !is.null(date.range) | !is.null(caught) | !is.null(gear) | !is.null(vessels) | !is.null(coords)) {
      fishsetjoinALL=paste0("INNER JOIN OBSERVER.ISFISHSETS
                      ON SPECIESSOUGHTCODES.SPECSCD_ID = ISFISHSETS.SPECSCD_ID")
    }
    #TRIPSJOIN - needed by trip, date, vessels
    if (!is.null(tripcode) | !is.null(date.range) | !is.null(vessels)) {
      tripjoinALL= "INNER JOIN OBSERVER.ISTRIPS
                      ON ISFISHSETS.TRIP_ID = ISTRIPS.TRIP_ID"
    }
    
    if (!is.null(setcode)) {
      settweak = paste0("AND ISFISHSETS.SETCD_ID IN (",setcode,")")
    }
    
    if (!is.null(tripcode)) {
      triptweak = paste0("AND ISTRIPS.TRIPCD_ID IN (",tripcode,")")
    }   
    
    if (!is.null(date.range)) {
      datetweak = paste0("AND to_date(to_char(isTrips.board_date,'YYYY'),'YYYY') BETWEEN to_date('",date.range[1],"','YYYY') AND to_date('",date.range[2],"','YYYY')")
    }
    
    if (!is.null(sought)) {
      soughttweak=paste0("AND SPECIESSOUGHTCODES.SPECSCD_ID IN (",sought,")")
    }
    
    if (!is.null(caught)) {
      caughttweak=paste0("AND ISCATCHES.SPECCD_ID IN (",caught,")")
      caughtjoin="INNER JOIN OBSERVER.ISCATCHES
      ON ISFISHSETS.FISHSET_ID = ISCATCHES.FISHSET_ID
      AND ISFISHSETS.SET_NO = ISCATCHES.SET_NO"
   }

    if(!is.null(gear)){
      geartweak=paste0("AND ISGEARS.GEARCD_ID IN (",gear,")")
      gearjoin="INNER JOIN OBSERVER.ISGEARS
      ON ISFISHSETS.GEAR_ID= ISGEARS.GEAR_ID"
      #fishsetjoin
    }
    
    if(!is.null(vessels)){
      vesseltweak=paste0("AND ISVESSELS.VESSEL_NAME IN (",vessels,")")
      vesseljoin="INNER JOIN OBSERVER.ISVESSELS
      ON ISTRIPS.VESS_ID = ISVESSELS.VESS_ID"
      #fishsetjoin
      #tripjoin
    }
    if (!is.null(coords)) {
      coordstweak = paste0("AND ISSETPROFILE.LONGITUDE BETWEEN ",coords[3]*-1," AND ",coords[4]*-1," AND ISSETPROFILE.latitude BETWEEN ",coords[2]," AND ",coords[1])
      coordsjoin = "INNER JOIN OBSERVER.ISSETPROFILE
      ON ISFISHSETS.FISHSET_ID = ISSETPROFILE.FISHSET_ID
      AND ISFISHSETS.SET_NO = ISSETPROFILE.SET_NO"
    }
    ######
    
    dynamic.where <- paste(settweak,
                           triptweak,
                           datetweak,
                           soughttweak,
                           caughttweak,
                           geartweak,
                           vesseltweak,
                           coordstweak, sep = " ")
    dynamic.join <-paste(fishsetjoinALL,
                         tripjoinALL,
                         setjoin,
                          tripjoin,
                          datejoin,
                          soughtjoin,
                          caughtjoin,
                          gearjoin,
                          vesseljoin,
                         coordsjoin, sep= " ")
    
    species.query = paste0("SELECT DISTINCT COMMON SOUGHT, SPECIESSOUGHTCODES.SPECSCD_ID
                         FROM OBSERVER.SPECIESSOUGHTCODES
                         ",dynamic.join,"
                          WHERE 1=1
                         ",dynamic.where,"
                          ORDER BY ",order)
  }
  the.species = sqlQuery(channel, species.query)
  return(the.species)
}

populate.caught.species <- function(order = "COMMON", setcode = NULL,tripcode = NULL, date.range = NULL, sought = NULL, caught = NULL, gear = NULL, vessels = NULL, coords = NULL) {
  if (order != "COMMON")
    order = "SPECCD_ID"
  if (is.null(setcode) & is.null(tripcode) & is.null(date.range) & is.null(sought) & is.null(caught) & is.null(gear) & is.null(vessels) & is.null(coords)){
  caught.species.query = paste0(
    "SELECT DISTINCT SPECIESCODES.COMMON CAUGHT,
    ISCATCHES.SPECCD_ID
    FROM OBSERVER.ISCATCHES
    INNER JOIN OBSERVER.SPECIESCODES
    ON SPECIESCODES.SPECCD_ID = ISCATCHES.SPECCD_ID
    ORDER BY ",order)
  }else{
    settweak=""
    triptweak=""
    datetweak=""
    soughttweak=""
    caughttweak=""
    geartweak=""
    vesseltweak=""
    coordstweak=""
    
    fishsetjoinALL=""
    tripjoinALL=""
    fishsetjoin=""
    setjoin=""
    tripjoin=""
    datejoin=""
    soughtjoin=""
    caughtjoin=""
    gearjoin=""
    vesseljoin=""
    coordsjoin=""
    
    #           INNER JOIN OBSERVER.ISSPECIESSOUGHTCODES
    #           ON ISFISHSETS.SPECSCD_ID = ISSPECIESSOUGHTCODES.SPECSCD_ID    
    
    ######
    #FISHSETSJOIN - needed by set, trip, date, sought, caught, gear,vessels
    if (!is.null(setcode) | !is.null(tripcode) | !is.null(date.range) | !is.null(sought)| !is.null(caught) | !is.null(gear) | !is.null(vessels)| !is.null(coords)) {
      fishsetjoinALL=paste0("INNER JOIN OBSERVER.ISFISHSETS
              ON ISCATCHES.FISHSET_ID = ISFISHSETS.FISHSET_ID
              AND ISCATCHES.SET_NO = ISFISHSETS.SET_NO")
    }
    #TRIPSJOIN - needed by trip, date, vessels
    if (!is.null(tripcode) | !is.null(date.range) | !is.null(vessels)) {
      tripjoinALL= "INNER JOIN OBSERVER.ISTRIPS
      ON ISFISHSETS.TRIP_ID = ISTRIPS.TRIP_ID"
    }
    
    if (!is.null(setcode)) {
      settweak = paste0("AND ISFISHSETS.SETCD_ID IN (",setcode,")")
    }
    
    if (!is.null(tripcode)) {
      triptweak = paste0("AND ISTRIPS.TRIPCD_ID IN (",tripcode,")")
    }   
    
    if (!is.null(date.range)) {
      datetweak = paste0("AND to_date(to_char(isTrips.board_date,'YYYY'),'YYYY') BETWEEN to_date('",date.range[1],"','YYYY') AND to_date('",date.range[2],"','YYYY')")
    }
    
    if (!is.null(sought)) {
      soughttweak=paste0("AND ISFISHSETS.SPECSCD_ID IN (",sought,")")
    }
    
     if (!is.null(caught)) {
       caughttweak=paste0("AND ISCATCHES.SPECCD_ID IN (",caught,")")
     }
    
    if(!is.null(gear)){
      geartweak=paste0("AND ISGEARS.GEARCD_ID IN (",gear,")")
      gearjoin="INNER JOIN OBSERVER.ISGEARS
      ON ISFISHSETS.GEAR_ID= ISGEARS.GEAR_ID"
    }
    
    if(!is.null(vessels)){
      vesseltweak=paste0("AND ISVESSELS.VESSEL_NAME IN (",vessels,")")
      vesseljoin="INNER JOIN OBSERVER.ISVESSELS
      ON ISTRIPS.VESS_ID = ISVESSELS.VESS_ID"
    }
    if (!is.null(coords)) {
      coordstweak = paste0("AND ISSETPROFILE.LONGITUDE BETWEEN ",coords[3]*-1," AND ",coords[4]*-1," AND ISSETPROFILE.latitude BETWEEN ",coords[2]," AND ",coords[1])
      coordsjoin = "INNER JOIN OBSERVER.ISSETPROFILE
        ON ISFISHSETS.FISHSET_ID = ISSETPROFILE.FISHSET_ID
        AND ISFISHSETS.SET_NO = ISSETPROFILE.SET_NO"
    }
    ######  
    dynamic.where <- paste(settweak,
                           triptweak,
                           datetweak,
                           soughttweak,
                           caughttweak,
                           geartweak,
                           vesseltweak,
                           coordstweak, sep = " ")
    dynamic.join <-paste(fishsetjoinALL,
                         tripjoinALL,
                         fishsetjoin,
                         setjoin,
                         tripjoin,
                         datejoin,
                         soughtjoin,
                         caughtjoin,
                         gearjoin,
                         vesseljoin,
                         coordsjoin, sep= " ")


    caught.species.query = caught.species.query = paste0("SELECT DISTINCT ISSPECIESCODES.COMMON CAUGHT,
          ISSPECIESCODES.SPECCD_ID
          FROM OBSERVER.ISSPECIESCODES
          INNER JOIN OBSERVER.ISCATCHES
          ON ISSPECIESCODES.SPECCD_ID = ISCATCHES.SPECCD_ID
          ",dynamic.join,"
          WHERE 1=1
          ",dynamic.where,"
          ORDER BY ",order)
  }
  the.caught.species <- sqlQuery(channel, caught.species.query)
  #found some tabs in the species names that were changing the order
  the.caught.species$CAUGHT <- gsub("\t","",the.caught.species$CAUGHT)
  return(the.caught.species)
}

populate.gear <- function(setcode = NULL,tripcode = NULL, date.range = NULL, sought = NULL, caught = NULL, gear = NULL, vessels = NULL, coords = NULL) {
  if (is.null(setcode) & is.null(tripcode) & is.null(date.range) & is.null(sought) & is.null(caught) & is.null(gear) & is.null(vessels) & is.null(coords)){
  gear.query = "SELECT DISTINCT ISGEARCODES.GEARCD_ID,
  ISGEARCODES.DESCRIPTION
  FROM OBSERVER.ISGEARCODES
  INNER JOIN OBSERVER.ISGEARS
  ON ISGEARCODES.GEARCD_ID = ISGEARS.GEARCD_ID
  ORDER BY ISGEARCODES.DESCRIPTION"
  }else{    
  settweak=""
  triptweak=""
  datetweak=""
  soughttweak=""
  caughttweak=""
  geartweak=""
  vesseltweak=""
  coordstweak=""
  
  fishsetjoinALL=""
  tripjoinALL=""
  fishsetjoin=""
  setjoin=""
  tripjoin=""
  datejoin=""
  soughtjoin=""
  caughtjoin=""
  gearjoin=""
  vesseljoin=""
  coordsjoin=""
  
  ######
# FISHSETS ALREADY JOINED
#   #FISHSETSJOIN - needed by set, trip, date, sought, caught, gear,vessels
  if (!is.null(setcode) | !is.null(tripcode) | !is.null(date.range) | !is.null(sought)| !is.null(caught) | !is.null(gear) | !is.null(vessels) | !is.null(coords)) {
    fishsetjoinALL=paste0("INNER JOIN OBSERVER.ISFISHSETS
                          ON ISFISHSETS.GEAR_ID = ISGEARS.GEAR_ID")
  }

    #TRIPSJOIN - needed by trip, date, vessels
  if (!is.null(tripcode) | !is.null(date.range) | !is.null(vessels)) {
    tripjoinALL= "INNER JOIN OBSERVER.ISTRIPS
    ON ISFISHSETS.TRIP_ID = ISTRIPS.TRIP_ID"
  }
  
  if (!is.null(setcode)) {
    settweak = paste0("AND ISFISHSETS.SETCD_ID IN (",setcode,")")
  }
  
  if (!is.null(tripcode)) {
    triptweak = paste0("AND ISTRIPS.TRIPCD_ID IN (",tripcode,")")
  }   
  
  if (!is.null(date.range)) {
    datetweak = paste0("AND to_date(to_char(isTrips.board_date,'YYYY'),'YYYY') BETWEEN to_date('",date.range[1],"','YYYY') AND to_date('",date.range[2],"','YYYY')")
  }
  
  if (!is.null(sought)) {
    soughttweak=paste0("AND ISFISHSETS.SPECSCD_ID IN (",sought,")")
  }
  
  if (!is.null(caught)) {
    caughttweak=paste0("AND ISCATCHES.SPECCD_ID IN (",caught,")")
    caughtjoin="INNER JOIN OBSERVER.ISCATCHES
    ON ISFISHSETS.FISHSET_ID = ISCATCHES.FISHSET_ID
    AND ISFISHSETS.SET_NO = ISCATCHES.SET_NO"
  }
  
  if(!is.null(gear)){
    geartweak=paste0("AND ISGEARCODES.GEARCD_ID IN (",gear,")")
  }
  
  if(!is.null(vessels)){
    vesseltweak=paste0("AND ISVESSELS.VESSEL_NAME IN (",vessels,")")
    vesseljoin="INNER JOIN OBSERVER.ISVESSELS
    ON ISTRIPS.VESS_ID = ISVESSELS.VESS_ID"
  }
  if (!is.null(coords)) {
    coordstweak = paste0("AND ISSETPROFILE.LONGITUDE BETWEEN ",coords[3]*-1," AND ",coords[4]*-1," AND ISSETPROFILE.latitude BETWEEN ",coords[2]," AND ",coords[1])
    coordsjoin = "INNER JOIN OBSERVER.ISSETPROFILE
        ON ISFISHSETS.FISHSET_ID = ISSETPROFILE.FISHSET_ID
        AND ISFISHSETS.SET_NO = ISSETPROFILE.SET_NO"
  }
  ######  
  
  dynamic.where <- paste(settweak,
                         triptweak,
                         datetweak,
                         soughttweak,
                         caughttweak,
                         geartweak,
                         vesseltweak,
                         coordstweak, sep = " ")
  dynamic.join <-paste(fishsetjoinALL,
                       tripjoinALL,
                       fishsetjoin,
                       setjoin,
                       tripjoin,
                       datejoin,
                       soughtjoin,
                       caughtjoin,
                       gearjoin,
                       vesseljoin,
                       coordsjoin, sep= " ")
    gear.query = paste0("SELECT DISTINCT ISGEARCODES.GEARCD_ID,
                        ISGEARCODES.DESCRIPTION
                        FROM OBSERVER.ISGEARCODES
                        INNER JOIN OBSERVER.ISGEARS
                        ON ISGEARCODES.GEARCD_ID = ISGEARS.GEARCD_ID
                        ",dynamic.join,"
                        WHERE 1=1
                        ",dynamic.where,"
                        ORDER BY ISGEARCODES.DESCRIPTION")
  }
  the.gear = sqlQuery(channel,gear.query)
  return(the.gear)
}

populate.vessels <- function(canadian.only = F, order = "VESSEL_NAME", setcode = NULL,tripcode = NULL, date.range = NULL, sought = NULL, caught = NULL, gear = NULL, vessels = NULL, coords = NULL) {
    if (order != "VESSEL_NAME")
    order = "CFV"
  if (canadian.only == T)
    cntry = "AND CTRYCD_ID IN (2,3)"
  else
    cntry = ""
  if (is.null(setcode) & is.null(tripcode) & is.null(date.range) & is.null(sought) & is.null(caught) & is.null(gear) & is.null(vessels) & is.null(coords)){
  vessel.query = paste0(
    "SELECT DISTINCT ISVESSELS.CFV,
    ISVESSELS.VESSEL_NAME
    FROM OBSERVER.ISVESSELS
    WHERE 1=1 ",cntry,"
    ORDER BY ",order) }
  else{
    settweak=""
    triptweak=""
    datetweak=""
    soughttweak=""
    caughttweak=""
    geartweak=""
    vesseltweak=""
    coordstweak=""
    
    fishsetjoinALL=""
    tripjoinALL=""
    fishsetjoin=""
    setjoin=""
    tripjoin=""
    datejoin=""
    soughtjoin=""
    caughtjoin=""
    gearjoin=""
    vesseljoin=""
    coordsjoin=""
    
    #TRIPSJOIN - needed by trip, date, vessels
    if (!is.null(setcode) | !is.null(tripcode) | !is.null(sought)| !is.null(caught)| !is.null(gear) |!is.null(date.range) | !is.null(vessels) | !is.null(coords)) {
      tripjoinALL= "INNER JOIN OBSERVER.ISTRIPS
      ON ISVESSELS.VESS_ID = ISTRIPS.VESS_ID"
    }
    
      #FISHSETSJOIN - needed by set, trip, date, sought, caught, gear,vessels
      if (!is.null(setcode) | !is.null(sought)| !is.null(caught) | !is.null(gear) | !is.null(vessels) | !is.null(coords)) {
        fishsetjoinALL=paste0("INNER JOIN OBSERVER.ISFISHSETS
                              ON ISTRIPS.TRIP_ID = ISFISHSETS.TRIP_ID")
      }

    
    if (!is.null(setcode)) {
      settweak = paste0("AND ISFISHSETS.SETCD_ID IN (",setcode,")")
    }
    
    if (!is.null(tripcode)) {
      triptweak = paste0("AND ISTRIPS.TRIPCD_ID IN (",tripcode,")")
    }   
    
    if (!is.null(date.range)) {
      datetweak = paste0("AND to_date(to_char(isTrips.board_date,'YYYY'),'YYYY') BETWEEN to_date('",date.range[1],"','YYYY') AND to_date('",date.range[2],"','YYYY')")
    }
    
    if (!is.null(sought)) {
      soughttweak=paste0("AND ISFISHSETS.SPECSCD_ID IN (",sought,")")
    }
    
    if (!is.null(caught)) {
      caughttweak=paste0("AND ISCATCHES.SPECCD_ID IN (",caught,")")
      caughtjoin="INNER JOIN OBSERVER.ISCATCHES
      ON ISFISHSETS.FISHSET_ID = ISCATCHES.FISHSET_ID
      AND ISFISHSETS.SET_NO = ISCATCHES.SET_NO"
    }
    
    if(!is.null(gear)){
      geartweak=paste0("AND ISGEARS.GEARCD_ID IN (",gear,")")
      gearjoin="INNER JOIN OBSERVER.ISGEARS
      ON ISFISHSETS.GEAR_ID= ISGEARS.GEAR_ID"
    }
    if (!is.null(coords)) {
      coordstweak = paste0("AND ISSETPROFILE.LONGITUDE BETWEEN ",coords[3]*-1," AND ",coords[4]*-1," AND ISSETPROFILE.latitude BETWEEN ",coords[2]," AND ",coords[1])
      coordsjoin = "INNER JOIN OBSERVER.ISSETPROFILE
      ON ISFISHSETS.FISHSET_ID = ISSETPROFILE.FISHSET_ID
      AND ISFISHSETS.SET_NO = ISSETPROFILE.SET_NO"
    }
    
    if(!is.null(vessels)){
      vesseltweak=paste0("AND ISVESSELS.VESSEL_NAME IN (",vessels,")")
    }
    ######     
    dynamic.where <- paste(settweak,
                           triptweak,
                           datetweak,
                           soughttweak,
                           caughttweak,
                           geartweak,
                           vesseltweak,
                           coordstweak, sep = " ")
    dynamic.join <-paste(tripjoinALL,
                         fishsetjoinALL,
                         fishsetjoin,
                         setjoin,
                         tripjoin,
                         datejoin,
                         soughtjoin,
                         caughtjoin,
                         gearjoin,
                         vesseljoin,
                         coordsjoin, sep= " ")
    
    vessel.query = paste0(
      "SELECT DISTINCT ISVESSELS.CFV,
    ISVESSELS.VESSEL_NAME
    FROM OBSERVER.ISVESSELS
    ",dynamic.join,"
    WHERE 1=1 
    ",dynamic.where,"
    ",cntry,"
    ORDER BY ",order)
  }
  the.vessels = sqlQuery(channel,vessel.query)
  return(the.vessels)
}

populate.year <- function(setcode = NULL,tripcode = NULL, date.range = NULL, sought = NULL, caught = NULL, gear = NULL, vessels = NULL, coords = NULL) {
    if (is.null(setcode) & is.null(tripcode) & is.null(date.range) & is.null(sought) & is.null(caught) & is.null(gear) & is.null(vessels)){
      the.year = c(format(Sys.Date(), "%Y"):1977)
    }else{
      settweak=""
      triptweak=""
      datetweak=""
      soughttweak=""
      caughttweak=""
      geartweak=""
      vesseltweak=""
      coordstweak=""
      
      fishsetjoinALL=""
      tripjoinALL=""
      fishsetjoin=""
      setjoin=""
      tripjoin=""
      datejoin=""
      soughtjoin=""
      caughtjoin=""
      gearjoin=""
      vesseljoin=""
      coordsjoin=""
      
      #FISHSETSJOIN - needed by set, trip, date, sought, caught, gear,vessels
      if (!is.null(setcode) | !is.null(sought)| !is.null(caught) | !is.null(gear) | !is.null(coords)  ) {
        fishsetjoinALL=paste0("INNER JOIN OBSERVER.ISFISHSETS
                              ON ISTRIPS.TRIP_ID = ISFISHSETS.TRIP_ID")
      }
      
      if (!is.null(sought)) {
        soughttweak = paste0("AND ISFISHSETS.SPECSCD_ID IN (",sought,")")
      }
      if (!is.null(caught)) {
        caughtjoin = "INNER JOIN OBSERVER.ISCATCHES
        ON ISFISHSETS.FISHSET_ID       = ISCATCHES.FISHSET_ID
        AND ISFISHSETS.SET_NO           = ISCATCHES.SET_NO"
        caughttweak = paste0("AND ISCATCHES.SPECCD_ID IN (",caught,")")
      }
      
      if (!is.null(gear)) {
        geartweak = paste0("AND ISGEARS.GEARCD_ID IN (",gear,")")
        gearjoin = "
        INNER JOIN OBSERVER.ISGEARS
        ON ISFISHSETS.GEAR_ID = ISGEARS.GEAR_ID"
      }
      if (!is.null(vessels)) {
        vesseltweak = paste0("AND ISVESSELS.VESSEL_NAME IN (",vessels,")")
        vesseljoin = "INNER JOIN OBSERVER.ISVESSELS
        ON ISTRIPS.VESS_ID = ISVESSELS.VESS_ID"
      }
      if (!is.null(date.range)) {
        datetweak = paste0("AND to_date(to_char(isTrips.board_date,'YYYY'),'YYYY') BETWEEN to_date('",date.range[1],"','YYYY') AND to_date('",date.range[2],"','YYYY')")
      }
      if (!is.null(coords)) {
        coordstweak = paste0("AND ISSETPROFILE.LONGITUDE BETWEEN ",coords[3]*-1," AND ",coords[4]*-1," AND ISSETPROFILE.latitude BETWEEN ",coords[2]," AND ",coords[1])
        coordsjoin = "INNER JOIN OBSERVER.ISSETPROFILE
        ON ISFISHSETS.FISHSET_ID = ISSETPROFILE.FISHSET_ID
        AND ISFISHSETS.SET_NO = ISSETPROFILE.SET_NO"
      }
      dynamic.where <- paste(settweak,
                             triptweak,
                             datetweak,
                             soughttweak,
                             caughttweak,
                             geartweak,
                             vesseltweak,
                             coordstweak, sep = " ")
      dynamic.join <-paste(fishsetjoinALL,
                           tripjoinALL,
                           fishsetjoin,
                           setjoin,
                           tripjoin,
                           datejoin,
                           soughtjoin,
                           caughtjoin,
                           gearjoin,
                           vesseljoin,
                           coordsjoin, sep= " ")
      
      year.query = paste0(
        "SELECT DISTINCT to_char(ISTRIPS.BOARD_DATE,'YYYY')
        FROM OBSERVER.ISTRIPS
        ",dynamic.join,"
        WHERE 1=1
        ",dynamic.where,"
        ORDER BY to_char(ISTRIPS.BOARD_DATE,'YYYY') DESC"
      )
      the.year = sqlQuery(channel,year.query)
      if (length(the.year[,1] > 1)) {
        the.year = the.year[,1]
      }
      }
    return(the.year)
    }

populate.setcode <- function(setcode = NULL,tripcode = NULL, date.range = NULL, sought = NULL, caught = NULL, gear = NULL, vessels = NULL, coords = NULL) {
    #If particular values are desired, we can filter the provided options
    if (is.null(setcode) & is.null(tripcode) & is.null(date.range) & is.null(sought) & is.null(caught) & is.null(gear) & is.null(vessels) & !is.null(coords)){
      setcode.query = "SELECT DISTINCT ISSETTYPECODES.SETCD_ID,
      ISSETTYPECODES.SET_TYPE
      FROM OBSERVER.ISSETTYPECODES
      ORDER BY SETCD_ID"
    }else{
      #if filters are to be applied, their default value is first set to ""
      #and then overwritten by an actual value
      
      settweak=""
      triptweak=""
      datetweak=""
      soughttweak=""
      caughttweak=""
      geartweak=""
      vesseltweak=""
      coordstweak=""
      
      fishsetjoinALL=""
      tripjoinALL=""
      fishsetjoin=""
      setjoin=""
      tripjoin=""
      datejoin=""
      soughtjoin=""
      caughtjoin=""
      gearjoin=""
      vesseljoin=""
      coordsjoin=""
 
      #FISHSETSJOIN - needed by set, trip, date, sought, caught, gear,vessels
      if (!is.null(setcode) | !is.null(tripcode) | !is.null(date.range) | !is.null(sought)| !is.null(caught) | !is.null(gear) | !is.null(vessels) | !is.null(coords) ) {
        fishsetjoinALL="INNER JOIN OBSERVER.ISFISHSETS
                              ON ISSETTYPECODES.SETCD_ID = ISFISHSETS.SETCD_ID"
      }
      
      if (!is.null(tripcode) | !is.null(vessels) | !is.null(date.range)) {
        tripjoinALL = "INNER JOIN OBSERVER.ISTRIPS
        ON ISFISHSETS.TRIP_ID   = ISTRIPS.TRIP_ID"
      }
      if (!is.null(tripcode)) {
        triptweak = paste0("AND ISTRIPS.TRIPCD_ID IN (",tripcode,")")
      }
      
      if (!is.null(vessels)) {
        vesseltweak = paste0("AND ISVESSELS.VESSEL_NAME IN (",vessels,")")
        vesseljoin = "INNER JOIN OBSERVER.ISVESSELS
        ON ISTRIPS.VESS_ID = ISVESSELS.VESS_ID"
      }
      
      if (!is.null(sought)) {
        soughttweak = paste0("AND ISFISHSETS.SPECSCD_ID IN (",sought,")")
      }
      
      if (!is.null(date.range)) {
        #between YYYY AND YYY wasn't getting full year, so had to implement so jiggery-pokery
        datetweak = paste0("AND to_date(to_char(isTrips.board_date,'YYYY'),'YYYY') BETWEEN to_date('",date.range[1],"','YYYY') AND to_date('",date.range[2],"','YYYY')")
      }
      
      if (!is.null(setcode)) {
        settweak = paste0("AND ISSETTYPECODES.SETCD_ID IN (",setcode,")")
      }
      
      if (!is.null(gear)) {
        geartweak = paste0("AND ISGEARS.GEARCD_ID IN (",gear,")")
        gearjoin = "INNER JOIN OBSERVER.ISGEARS
        ON ISFISHSETS.GEAR_ID = ISGEARS.GEAR_ID"
      }
      if (!is.null(coords)) {
        coordstweak = paste0("AND ISSETPROFILE.LONGITUDE BETWEEN ",coords[3]*-1," AND ",coords[4]*-1," AND ISSETPROFILE.latitude BETWEEN ",coords[2]," AND ",coords[1])
        coordsjoin = "INNER JOIN OBSERVER.ISSETPROFILE
        ON ISFISHSETS.FISHSET_ID = ISSETPROFILE.FISHSET_ID
        AND ISFISHSETS.SET_NO = ISSETPROFILE.SET_NO"
      }
      
      dynamic.where <- paste(settweak,
                             triptweak,
                             datetweak,
                             soughttweak,
                             caughttweak,
                             geartweak,
                             vesseltweak,
                             coordstweak, sep = " ")
      dynamic.join <-paste(fishsetjoinALL,
                           tripjoinALL,
                           fishsetjoin,
                           setjoin,
                           tripjoin,
                           datejoin,
                           soughtjoin,
                           caughtjoin,
                           gearjoin,
                           vesseljoin,
                           coordsjoin, sep= " ")
      
      setcode.query <- paste0(
        "SELECT DISTINCT ISSETTYPECODES.SETCD_ID,
        ISSETTYPECODES.SET_TYPE
        FROM OBSERVER.ISSETTYPECODES
        ",dynamic.join,"
        WHERE 1=1
        ",dynamic.where,"
        ORDER BY SETCD_ID"
      )
    }
    the.setcode = sqlQuery(channel,setcode.query)
    return(the.setcode)
      }

populate.tripcode <- function(setcode = NULL,tripcode = NULL, date.range = NULL, sought = NULL, caught = NULL, gear = NULL, vessels = NULL, coords = NULL) {
    if (is.null(setcode) & is.null(tripcode) & is.null(date.range) & is.null(sought) & is.null(caught) & is.null(gear) & is.null(vessels) & is.null(coords)){
      tripcode.query = "SELECT DISTINCT ISTRIPTYPECODES.TRIPCD_ID,
      ISTRIPTYPECODES.TRIP_TYPE
      FROM OBSERVER.ISTRIPTYPECODES
      ORDER BY TRIPCD_ID"
    } else{
      
      settweak=""
      triptweak=""
      datetweak=""
      soughttweak=""
      caughttweak=""
      geartweak=""
      vesseltweak=""
      loctweak=""
      coordstweak=""
      
      fishsetjoinALL=""
      tripjoinALL=""
      fishsetjoin=""
      setjoin=""
      tripjoin=""
      datejoin=""
      soughtjoin=""
      caughtjoin=""
      gearjoin=""
      vesseljoin=""
      locjoin=""
      coordsjoin=""
      
      if (!is.null(setcode) | !is.null(tripcode) | !is.null(date.range) | !is.null(sought)| !is.null(caught) | !is.null(gear) | !is.null(vessels) | !is.null(coords)) {
        tripjoinALL = "INNER JOIN OBSERVER.ISTRIPS
        ON ISTRIPTYPECODES.TRIPCD_ID   = ISTRIPS.TRIPCD_ID"
      }
      if (!is.null(setcode) | !is.null(tripcode) | !is.null(sought)| !is.null(caught) | !is.null(gear) | !is.null(coords)) {
        fishsetjoinALL = "INNER JOIN OBSERVER.ISFISHSETS
        ON ISTRIPS.TRIP_ID   = ISFISHSETS.TRIP_ID"
      }
      
      if (!is.null(setcode)) {
        settweak = paste0("AND ISFISHSETS.SETCD_ID IN (",setcode,")")
      }
      
      if (!is.null(sought)) {
        soughttweak = paste0("AND ISFISHSETS.SPECSCD_ID IN (",sought,")")
      }
      
      if (!is.null(gear)) {
        geartweak = paste0("AND ISGEARS.GEARCD_ID IN (",gear,")")
        gearjoin = "INNER JOIN OBSERVER.ISGEARS
        ON ISFISHSETS.GEAR_ID = ISGEARS.GEAR_ID"
      }
      if (!is.null(date.range)) {
        #between YYYY AND YYY wasn't getting full year, so had to implement so jiggery-pokery
        datetweak =  paste0(
          "AND to_date(to_char(ISTRIPS.board_date,'YYYY'),'YYYY') BETWEEN to_date('",date.range[1],"','YYYY') AND to_date('",date.range[2],"','YYYY')"
        )
      }
      
      if (!is.null(tripcode)) {
        triptweak = paste0("AND ISTRIPTYPECODES.TRIPCD_ID IN (",tripcode,")")
      }
      
      if (!is.null(vessels)) {
        vesseltweak = paste0("AND ISVESSELS.VESSEL_NAME IN (",vessels,")")
        vesseljoin = "INNER JOIN OBSERVER.ISVESSELS
        ON ISTRIPS.VESS_ID = ISVESSELS.VESS_ID"
      }
      
      if (!is.null(coords)) {
        coordstweak = paste0("AND ISSETPROFILE.LONGITUDE BETWEEN ",coords[3]*-1," AND ",coords[4]*-1," AND ISSETPROFILE.latitude BETWEEN ",coords[2]," AND ",coords[1])
        coordsjoin = "INNER JOIN OBSERVER.ISSETPROFILE
        ON ISFISHSETS.FISHSET_ID = ISSETPROFILE.FISHSET_ID
        AND ISFISHSETS.SET_NO = ISSETPROFILE.SET_NO"
      }
      
      dynamic.where <- paste(settweak,
                             triptweak,
                             datetweak,
                             soughttweak,
                             caughttweak,
                             geartweak,
                             vesseltweak,
                             coordstweak, sep = " ")
      dynamic.join <-paste(tripjoinALL,
                           fishsetjoinALL,
                           fishsetjoin,
                           setjoin,
                           tripjoin,
                           datejoin,
                           soughtjoin,
                           caughtjoin,
                           gearjoin,
                           vesseljoin,
                           coordsjoin, sep= " ")
      
      tripcode.query = paste(
        "SELECT DISTINCT ISTRIPTYPECODES.TRIPCD_ID,
        ISTRIPTYPECODES.TRIP_TYPE
        FROM OBSERVER.ISTRIPTYPECODES
        ",dynamic.join,"
        WHERE 1=1
        ",dynamic.where,"
        ORDER BY TRIPCD_ID"
      )
    }
    the.tripcode = sqlQuery(channel,tripcode.query)
    return(the.tripcode)
      }
