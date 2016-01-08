get.species<-function(){
  species.query="SELECT DISTINCT COMMON, SPECSCD_ID
  FROM SPECIESSOUGHTCODES
  ORDER BY COMMON"
  the.species = sqlQuery(channel, species.query)
  return(the.species)
}
get.gear<-function(){
  gear.query="SELECT DISTINCT ISGEARCODES.GEARCD_ID,
ISGEARCODES.DESCRIPTION
FROM ISGEARCODES
ORDER BY DESCRIPTION"
  the.gear = sqlQuery(channel,gear.query)
  return(the.gear)
}
get.setcode<-function(tripcode=NULL,sought=NULL, date.range=NULL){
  #If particular values are desired, we can filter the provided options
  if (is.null(tripcode) & is.null(sought) & is.null(date.range) ){
    setcode.query="SELECT DISTINCT ISSETTYPECODES.SETCD_ID,
    ISSETTYPECODES.SET_TYPE
    FROM ISSETTYPECODES
    ORDER BY SETCD_ID"
  }else{
    #if filters are to be applied, their default value is first set to ""
    #and then overwritten by an actual value
    triptweak=""
    soughttweak=""
    datetweak=""
    if (!is.null(tripcode)) triptweak=paste0("AND ISTRIPTYPECODES.TRIPCD_ID IN (",tripcode,")")
    if (!is.null(sought)) soughttweak=paste0("AND ISSPECIESCODES.SPECCD_ID IN (",sought,")")
    if (!is.null(date.range)) datetweak =  paste0("AND ISTRIPS.board_date BETWEEN to_date('",date.range[1],"','YYYY-MM-DD') AND to_date('",date.range[2],"','YYYY-MM-DD')")
    
    setcode.query=paste0("SELECT DISTINCT ISFISHSETS.SETCD_ID,
    ISSETTYPECODES.SET_TYPE
    FROM ISTRIPTYPECODES
    INNER JOIN ISTRIPS
    ON ISTRIPTYPECODES.TRIPCD_ID = ISTRIPS.TRIPCD_ID
    INNER JOIN ISFISHSETS
    ON ISTRIPS.TRIP_ID = ISFISHSETS.TRIP_ID
    INNER JOIN ISSETTYPECODES
    ON ISFISHSETS.SETCD_ID          = ISSETTYPECODES.SETCD_ID
    INNER JOIN ISSPECIESCODES
    ON ISSPECIESCODES.SPECCD_ID  = ISFISHSETS.SPECSCD_ID
    WHERE 1=1
    ",triptweak,"
    ",soughttweak,"
    ",datetweak,"
    ORDER BY SETCD_ID")
  }
  the.setcode = sqlQuery(channel,setcode.query)
  return(the.setcode)
}
get.tripcode<-function(setcode=NULL,sought=NULL, date.range=NULL){
  #If particular values are desired, we can filter the provided options
  if (is.null(setcode) & is.null(sought) & is.null(date.range) ){
    tripcode.query="SELECT DISTINCT ISTRIPTYPECODES.TRIPCD_ID,
    ISTRIPTYPECODES.TRIP_TYPE
    FROM ISTRIPTYPECODES
    ORDER BY TRIPCD_ID"
  } else{
    settweak=""
    soughttweak=""
    datetweak=""
    if (!is.null(setcode)) triptweak=paste0("AND ISFISHSETS.SETCD_ID IN (",setcode,")")
    if (!is.null(sought)) soughttweak=paste0("AND ISSPECIESCODES.SPECCD_ID IN (",sought,")")
    if (!is.null(date.range)) datetweak =  paste0("AND ISTRIPS.board_date BETWEEN to_date('",date.range[1],"','YYYY-MM-DD') AND to_date('",date.range[2],"','YYYY-MM-DD')")
    
    tripcode.query=paste("SELECT DISTINCT ISTRIPTYPECODES.TRIPCD_ID,
    ISTRIPTYPECODES.TRIP_TYPE
    FROM ISTRIPTYPECODES
    INNER JOIN ISTRIPS
    ON ISTRIPTYPECODES.TRIPCD_ID = ISTRIPS.TRIPCD_ID
    INNER JOIN ISFISHSETS
    ON ISTRIPS.TRIP_ID        = ISFISHSETS.TRIP_ID
    INNER JOIN ISSPECIESCODES
    ON ISSPECIESCODES.SPECCD_ID  = ISFISHSETS.SPECSCD_ID
    WHERE 1=1
     ",settweak,"
     ",soughttweak,"
    ",datetweak,"
    ORDER BY TRIPCD_ID")
  }
  the.tripcode = sqlQuery(channel,tripcode.query)
  return(the.tripcode)
}
