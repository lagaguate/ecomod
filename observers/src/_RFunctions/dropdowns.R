get.species<-function(order="COMMON"){
  if (order!="COMMON") order="SPECSCD_ID"
  species.query=paste0("SELECT DISTINCT COMMON, SPECSCD_ID
  FROM SPECIESSOUGHTCODES
  ORDER BY ",order)
  the.species = sqlQuery(channel, species.query)
  return(the.species)
}

get.caught.species<-function(order="COMMON"){
  if (order!="COMMON") order="SPECCD_ID"
  caught.species.query=paste0("SELECT DISTINCT SPECIESCODES.COMMON,
  ISCATCHES.SPECCD_ID
  FROM ISCATCHES
  INNER JOIN SPECIESCODES
  ON SPECIESCODES.SPECCD_ID = ISCATCHES.SPECCD_ID
  ORDER BY ",order)
  the.caught.species <- sqlQuery(channel, caught.species.query)
  #found some tabs in the species names that were changing the order
  the.caught.species$COMMON<-gsub("\t","",the.caught.species$COMMON)
  return(the.caught.species)
}
get.gear<-function(){
  gear.query="SELECT DISTINCT ISGEARCODES.GEARCD_ID,
  ISGEARCODES.DESCRIPTION
  FROM ISFISHSETS
  INNER JOIN ISGEARS
  ON ISFISHSETS.GEAR_ID = ISGEARS.GEAR_ID
  INNER JOIN ISGEARCODES
  ON ISGEARS.GEARCD_ID       = ISGEARCODES.GEARCD_ID
  ORDER BY ISGEARCODES.DESCRIPTION"
  the.gear = sqlQuery(channel,gear.query)
  return(the.gear)
}
get.year<-function(sought=NULL, caught=NULL, gear=NULL){
  if (is.null(sought) & is.null(caught) & is.null(gear)){
  the.year=c(format(Sys.Date(), "%Y"):1977)
  }else{
    soughttweak=""
    caughtjoin=""
    caughttweak=""
    geartweak=""
    gearjoin=""
    if (!is.null(sought)) {
      soughttweak=paste0("AND ISFISHSETS.SPECSCD_ID IN (",sought,")")
    }
    if (!is.null(caught)) {
      caughtjoin="INNER JOIN ISCATCHES 
                  ON ISFISHSETS.FISHSET_ID       = ISCATCHES.FISHSET_ID
                  AND ISFISHSETS.SET_NO           = ISCATCHES.SET_NO"
    caughttweak=paste0("AND ISCATCHES.SPECCD_ID IN (",caught,")")
    }
    if (!is.null(gear)) {
    geartweak=paste0("AND ISGEARCODES.GEARCD_ID IN (",gear,")")
    gearjoin = "
    INNER JOIN ISGEARS
    ON ISFISHSETS.GEAR_ID = ISGEARS.GEAR_ID
    INNER JOIN ISGEARCODES
    ON ISGEARS.GEARCD_ID        = ISGEARCODES.GEARCD_ID"
    }
    year.query=paste0("SELECT DISTINCT to_char(ISTRIPS.BOARD_DATE,'YYYY')
                      FROM ISFISHSETS
                      INNER JOIN ISTRIPS
                      ON ISFISHSETS.TRIP_ID       = ISTRIPS.TRIP_ID
                      ",gearjoin,"
                      ",caughtjoin,"
                      WHERE 1=1 
                      ",soughttweak,"
                      ",geartweak,"
                      ",caughttweak,"
                      ORDER BY to_char(ISTRIPS.BOARD_DATE,'YYYY') DESC")
    the.year = sqlQuery(channel,year.query)
    if (length(the.year[,1]>1)){
      the.year=the.year[,1]
    }
  }
  #years<-c(format(Sys.Date(), "%Y"):1977)
  #the.year = sqlQuery(channel,gear.query)

  return(the.year)
}
get.setcode<-function(tripcode=NULL,sought=NULL, date.range=NULL, gear=NULL){
  #If particular values are desired, we can filter the provided options
  if (is.null(tripcode) & is.null(sought) & is.null(date.range) & is.null(gear) ){
    setcode.query="SELECT DISTINCT ISSETTYPECODES.SETCD_ID,
    ISSETTYPECODES.SET_TYPE
    FROM ISSETTYPECODES
    ORDER BY SETCD_ID"
  }else{
    #if filters are to be applied, their default value is first set to ""
    #and then overwritten by an actual value
    triptweak=""
    tripjoin=""
    soughttweak=""
    soughtjoin=""
    datetweak=""
    datejoin=""
    geartweak=""
    gearjoin=""
    if (!is.null(tripcode)) {
      triptweak=paste0("AND ISTRIPS.TRIPCD_ID IN (",tripcode,")")
      tripjoin= "
      INNER JOIN ISTRIPS
      ON ISFISHSETS.TRIP_ID   = ISTRIPS.TRIP_ID"
    }
    if (!is.null(sought)) {
      soughttweak=paste0("AND ISFISHSETS.SPECSCD_ID IN (",sought,")")
    }
    if (!is.null(date.range)) {
      #between YYYY AND YYY wasn't getting full year, so had to implement so jiggery-pokery
      datetweak =  paste0("AND to_date(to_char(ISTRIPS.board_date,'YYYY'),'YYYY') BETWEEN to_date('",date.range[1],"-01-01','YYYY-MM-DD') AND to_date('",date.range[2],"-12-31','YYYY-MM-DD')")
      datejoin = "
      INNER JOIN ISTRIPS
      ON ISFISHSETS.TRIP_ID   = ISTRIPS.TRIP_ID"
    }
    if (!is.null(gear)) {
      geartweak=paste0("AND ISGEARCODES.GEARCD_ID IN (",gear,")")
      gearjoin = "
      INNER JOIN ISGEARS
      ON ISFISHSETS.GEAR_ID = ISGEARS.GEAR_ID
      INNER JOIN ISGEARCODES
      ON ISGEARS.GEARCD_ID        = ISGEARCODES.GEARCD_ID"
    }
    
    setcode.query=paste0("SELECT DISTINCT ISFISHSETS.SETCD_ID,
    ISSETTYPECODES.SET_TYPE
    FROM ISSETTYPECODES
    INNER JOIN ISFISHSETS
    ON ISSETTYPECODES.SETCD_ID = ISFISHSETS.SETCD_ID
    ",tripjoin,"
    ",datejoin,"
    ",gearjoin,"
    WHERE 1=1
    ",triptweak,"
    ",soughttweak,"
    ",datetweak,"
    ",geartweak,"
    ORDER BY SETCD_ID")
  }
  the.setcode = sqlQuery(channel,setcode.query)
  return(the.setcode)
}
get.tripcode<-function(setcode=NULL,sought=NULL, date.range=NULL, gear=NULL){
  #If particular values are desired, we can filter the provided options
  if (is.null(setcode) & is.null(sought) & is.null(date.range) & is.null(gear) ){
    tripcode.query="SELECT DISTINCT ISTRIPTYPECODES.TRIPCD_ID,
    ISTRIPTYPECODES.TRIP_TYPE
    FROM ISTRIPTYPECODES
    ORDER BY TRIPCD_ID"
  } else{
    settweak=""
    setjoin=""
    soughttweak=""
    soughtjoin=""
    datetweak=""
    geartweak=""
    gearjoin=""
    fishsetjoin=""
    if (!is.null(setcode)) {
      settweak=paste0("AND ISFISHSETS.SETCD_ID IN (",setcode,")")
    }
    if (!is.null(sought)) {
      soughttweak=paste0("AND ISFISHSETS.SPECSCD_ID IN (",sought,")")
    }
    if (!is.null(gear)) {
      geartweak=paste0("AND ISGEARCODES.GEARCD_ID IN (",gear,")")
      gearjoin = "
      INNER JOIN ISGEARS
      ON ISFISHSETS.GEAR_ID = ISGEARS.GEAR_ID
      INNER JOIN ISGEARCODES
      ON ISGEARS.GEARCD_ID        = ISGEARCODES.GEARCD_ID"
    }
    if (length(soughttweak)>0 | length(settweak)>0 | length(geartweak)>0){
      fishsetjoin="INNER JOIN ISFISHSETS
      ON ISFISHSETS.TRIP_ID   = ISTRIPS.TRIP_ID"
    }
    if (!is.null(date.range)) {
      #between YYYY AND YYY wasn't getting full year, so had to implement so jiggery-pokery
      datetweak =  paste0("AND to_date(to_char(ISTRIPS.board_date,'YYYY'),'YYYY') BETWEEN to_date('",date.range[1],"','YYYY') AND to_date('",date.range[2],"','YYYY')")
    }
      
    tripcode.query=paste("SELECT DISTINCT ISTRIPTYPECODES.TRIPCD_ID,
    ISTRIPTYPECODES.TRIP_TYPE
    FROM ISTRIPTYPECODES
    INNER JOIN ISTRIPS
    ON ISTRIPTYPECODES.TRIPCD_ID = ISTRIPS.TRIPCD_ID
    ",fishsetjoin,"
    ",soughtjoin,"
    ",gearjoin,"
    WHERE 1=1
     ",settweak,"
     ",soughttweak,"
     ",datetweak,"
     ",geartweak,"
    ORDER BY TRIPCD_ID")
  }
  the.tripcode = sqlQuery(channel,tripcode.query)
  return(the.tripcode)
}
