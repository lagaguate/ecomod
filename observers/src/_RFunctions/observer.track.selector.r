if (F) {
  # load required ecomod functions
  loadfunctions("observers/src/_RFunctions")
  loadfunctions("utility/src/_Rfunctions/data.manipulation")
  loadfunctions("utility/src/_Rfunctions/datetime")
  loadfunctions("utility/src/_Rfunctions/sql.tools")
  #connect to db
  library(RODBC)
  channel <-  odbcConnect("PTRAN",uid = oracle.personal.username,pwd = oracle.personal.password)
}
observer.track.selector<-function() {
  library(sqldf)
  
  theFilters = c(
    "By Species Sought",
    "By Species Caught",
    "By Gear",
    "By Vessel",
    "By Date Range",
    "By Set Code",
    "By Trip Code",
    "By Location",
    "All Done"
  )
  this.Filter = 1
  
  #clear out any existing values
  setcode = NULL
  tripcode = NULL
  date.range = NULL
  sought = NULL
  caught = NULL
  gear = NULL
  vessels = NULL
  coords = NULL
  
  soughtQ = ""
  catchQ = ""
  gearQ = ""
  vesselsQ = ""
  dateQ = ""
  setcodeQ = ""
  tripcodeQ = ""
  locQ = ""
  
  catchfield = ""
  catchtable = ""
  catchjoin = ""
  the.species <- sqlQuery(channel, "SELECT DISTINCT COMMON SOUGHT, SPECSCD_ID FROM OBSERVER.SPECIESSOUGHTCODES"
    )
  print("### Description ###")
  print("Displayed is a list of potential filters you can apply to the data")
  print("You can apply any or all of these, in the order you choose")
  print("Prior selections will modify subsequent ones (e.g. selecting 'traps' ")
  print("in gear will limit the available species to those that are typically caught in traps")
  print("Choose 'cancel' or 'All Done' to apply your selections")
  print("###############")
  
  while (length(theFilters > 0)) {
    this.Filter <-  select.list((theFilters),
                                multiple = F, graphics = T,
                                title = "Choose the Filter to Apply"
    )
    if (this.Filter == "" | this.Filter == "All Done") {
      break
    }else if (this.Filter == "By Species Sought") {
      sought = get.sought.species(setcode = setcode, tripcode = tripcode, date.range = date.range, sought = sought, caught = caught, gear = gear, vessels = vessels, coords = coords)
      if (sought != "") {
        soughtQ = paste0("AND f.specscd_id IN (",sought,")")
        theFilters <- theFilters[theFilters != "By Species Sought"]
      }
    }else if (this.Filter == "By Species Caught") {
      caught = get.caught.species(setcode = setcode, tripcode = tripcode, date.range = date.range, sought = sought, caught = caught, gear = gear, vessels = vessels, coords = coords)
      if (caught != "") {
        catchQ = paste0("AND ISCATCHES.SPECCD_ID IN (",caught,")")
        theFilters <- theFilters[theFilters != "By Species Caught"]
      }
    }else if (this.Filter == "By Gear") {
      gear = get.gear(setcode = setcode, tripcode = tripcode, date.range = date.range, sought = sought, caught = caught, gear = gear, vessels = vessels, coords = coords)
      if (gear != "") {
        gearQ <- paste0("AND g.gearcd_id IN (",gear,")")
        theFilters <- theFilters[theFilters != "By Gear"]
      }
    }else if (this.Filter == "By Vessel") {
      vessels = get.vessels(setcode = setcode, tripcode = tripcode, date.range = date.range, sought = sought, caught = caught, gear = gear, vessels = vessels, coords = coords)
      if (vessels != "") {
        vesselsQ <- paste0("AND v.VESSEL_NAME IN (",vessels,")")
        theFilters <- theFilters[theFilters != "By Vessel"]
      }
    }else if (this.Filter == "By Set Code") {
      setcode = get.setcode(setcode = setcode, tripcode = tripcode, date.range = date.range, sought = sought, caught = caught, gear = gear, vessels = vessels, coords = coords)
      if (setcode != "") {
        setcodeQ <- paste0("AND f.setcd_id IN (",setcode,")")
        theFilters <- theFilters[theFilters != "By Set Code"]
      }
    }else if (this.Filter == "By Trip Code") {
      tripcode = get.tripcode(setcode = setcode, tripcode = tripcode, date.range = date.range, sought = sought, caught = caught, gear = gear, vessels = vessels, coords = coords)
      if (tripcode != "") {
        tripcodeQ <- paste0("AND t.tripcd_id IN (",tripcode,")")
        theFilters <- theFilters[theFilters != "By Trip Code"]
      }
    }else if (this.Filter == "By Date Range") {
      date.range <-
        get.date.range(setcode = setcode, tripcode = tripcode, date.range = date.range, sought = sought, caught = caught, gear = gear, vessels = vessels, coords = coords)
      #need dateQ
      if (date.range[1] != "" & date.range[2] != "") {
        dateQ <-
          paste0(
            "AND to_date(to_char(t.board_date,'YYYY'),'YYYY') BETWEEN to_date('",date.range[1],"','YYYY') AND to_date('",date.range[2],"','YYYY')"
          )
        theFilters <- theFilters[theFilters != "By Date Range"]
      }
    }else if (this.Filter == "By Location") {
      coords <- get.location()
      if (coords[1] != "" &
          coords[2] != "" & coords[3] != "" & coords[4] != "") {
        locQ = paste0(
          "AND (
          (p1.longitude*-1 BETWEEN ",coords[4]," AND ",coords[3]," AND p1.latitude BETWEEN ",coords[2]," AND ",coords[1],")
          or (p2.longitude*-1 BETWEEN ",coords[4]," AND ",coords[3]," AND p2.latitude BETWEEN ",coords[2]," AND ",coords[1],")
          or (p3.longitude*-1 BETWEEN ",coords[4]," AND ",coords[3]," AND p3.latitude BETWEEN ",coords[2]," AND ",coords[1],")
          or (p4.longitude*-1 BETWEEN ",coords[4]," AND ",coords[3]," AND p4.latitude BETWEEN ",coords[2]," AND ",coords[1],"))"
    )
        theFilters <- theFilters[theFilters != "By Location"]
      }
    }
    }
  
  
  caught.Filter=""
  if (nchar(catchQ) < 1){
    print("### Warning ###")
    print("Adding caught species to your results generally greatly increases")
    print("the number of returned records.  Say 'No' unless you truly need them")
    print("###############")
    caught.Filter <-  select.list(
      c("No","Yes"),
      multiple = F, graphics = T,
      preselect = "No",
      title = "Add CAUGHT data (e.g. bycatch)?"
    )
  }

  if (caught.Filter == "Yes" | nchar(catchQ) > 0 ) {
    catchfield = ", ISCATCHES.SPECCD_ID"
    catchtable = ", OBSERVER.ISCATCHES"
    catchjoin = "AND f.FISHSET_ID       = ISCATCHES.FISHSET_ID
    AND f.SET_NO           = ISCATCHES.SET_NO"
    the.caught.species <- populate.caught.species(setcode = setcode, tripcode = tripcode, date.range = date.range, sought = sought, caught = caught, gear = gear, vessels = vessels, coords = coords)
    caught.Filter = "Yes"
    }
  
  print("Trying to apply your filters...")
  
  dynamic.fields <- paste(catchfield, sep = " ")
  dynamic.tables <- paste(catchtable, sep = " ")
  dynamic.joins <- paste(catchjoin, sep = " ")
  
  dynamic.where <- paste(soughtQ,
                         gearQ,
                         vesselsQ,
                         setcodeQ,
                         tripcodeQ,
                         dateQ,
                         catchQ,
                         locQ, sep = " ")
  #between YYYY AND YYY wasn't getting full year, so had to implement so jiggery-pokery
  ISD_INF_query <<-
    paste0(
      "SELECT DISTINCT SUBSTR(v.vessel_name,1,15) vessel,
      t.tripcd_id,
      to_char(t.board_date,'YYYY') year,
      t.board_date,
      v.cfv,
      v.ctrycd_id,
      t.trip,
      f.set_no,
      f.nafarea_id,
      f.stratum_id,
      f.station,
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
      ",dynamic.fields,"
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
      ",dynamic.tables,"
      WHERE f.fishset_id=p1.f_id
      AND f.fishset_id  =p2.f_id
      AND f.fishset_id  =p3.f_id
      AND f.fishset_id  =p4.f_id
      AND t.vess_id     =v.vess_id
      AND t.trip_id     =f.trip_id
      AND g.gear_id     =f.gear_id
      AND f.setcd_id    = a.setcd_id
      ",dynamic.joins,"
      ",dynamic.where
    )
  ISD_INF <<- sqlQuery(channel,ISD_INF_query)
  if (is.character(ISD_INF)) {
    print("Something is wrong with your query")
    stop(return(ISD_INF_query))
  }else{
    recno = nrow(ISD_INF)
    if (recno < 1) {
      print("Error: No data can be found for your selection")
      return(ISD_INF_query)
    }else{
      print(paste0("Found ", recno," records."))
      #crashed computer enough times to desire this chance to kill function
      if (recno > 1000) {
        print("Proceed? 5000 records can take a couple minutes")
        cont = readline("Enter Y to continue: ")
        if (toupper(cont) != "Y") {
          rm(ISD_INF)
          print("Discarded that data")
          stop(return(ISD_INF_query))
        }
      }
    }
    if (caught.Filter == "Yes") {
        ISD_INF = merge(ISD_INF,the.caught.species)
    }
    ISD_INF = merge(ISD_INF,the.species)
    
    print("Identifying tracks...")
    p1.dat <- sqldf("SELECT DISTINCT FISHSET_ID, 1 ORD, P1LONG X, P1LAT Y FROM ISD_INF")
    p1.dat <- p1.dat[complete.cases(p1.dat),]
    p2.dat <- sqldf("SELECT  DISTINCT FISHSET_ID, 2 ORD, P2LONG X, P2LAT Y FROM ISD_INF")
    p2.dat <- p2.dat[complete.cases(p2.dat),]
    p3.dat <- sqldf("SELECT  DISTINCT FISHSET_ID, 3 ORD, P3LONG X, P3LAT Y FROM ISD_INF")
    p3.dat <- p3.dat[complete.cases(p3.dat),]
    p4.dat <- sqldf("SELECT  DISTINCT FISHSET_ID, 4 ORD, P4LONG X, P4LAT Y FROM ISD_INF")
    p4.dat <- p4.dat[complete.cases(p4.dat),]
    
    #could combine the various combinations of points to get potential tracks
    #p1-p2:set.deploy
    #p2-p3:set.tow
    #p3-p4:set.retrieve
    #p1-p4:set.all
    
    set.tow <- rbind(p2.dat,p3.dat)
#     set.tow.segs <- set.tow[duplicated(set.tow$FISHSET_ID),]$FISHSET_ID
#     set.tow <- set.tow[set.tow$FISHSET_ID %in% set.tow.segs,]
    set.tow <- set.tow[order(set.tow$FISHSET_ID,set.tow$ORD),]

    set.all <- rbind(p1.dat,p2.dat,p3.dat,p4.dat)
#     set.all.segs <- set.all[duplicated(set.all$FISHSET_ID),]$FISHSET_ID
#     set.all <- set.all[set.all$FISHSET_ID %in% set.all.segs,]
    set.all <- set.all[order(set.all$FISHSET_ID,set.all$ORD),]
 
    set.all.attrib <- sqldf("SELECT * FROM ISD_INF")

    set.tow <- merge(set.tow,set.all.attrib)
    set.all <- merge(set.all,set.all.attrib)
    
    results = list(ISD_INF_query, set.tow, set.all)
  }
  print(paste("Completed"))
  return(results)
  }
# #run it
# test<-observer.track.selector()
