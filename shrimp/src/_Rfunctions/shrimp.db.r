
  shrimp.db = function( DS=NULL ) {

    if (DS %in% c("shrimp.marfis", "shrimp.marfis.redo") ) {
      fn = file.path( shrimp.data, "shrimp.marfis.datadump.rdata" )
      dir.create( dirname( fn ), recursive=T, showWarnings=F)  
      if ( DS=="shrimp.marfis" ) {
        load( fn )
        return ( x )
      }
      query = paste(
        "select VESSEL_NAME, VR_NUMBER, LICENCE_ID, TRIP_NUMBER,",
          "DATE_FISHED, DATE_LANDED, LATITUDE, LONGITUDE, END_LATITUDE," , 
          "END_LONGITUDE, DET_SHRIMP_FISHING_AREA, AVERAGE_DEPTH, TOW_DURATION_HOURS," ,
          "TOW_DURATION_MINUTES, RND_WEIGHT_KGS, COUNT_PER_LB" ,
        "from MARFISSCI.MOBILE_SHRIMP", 
        "ORDER BY VESSEL_NAME, DATE_FISHED, TRIP_NUMBER, TOW" )
		
	    print("WARNING --- this needs to be run from an R environment on a windows machine")
	    require(RODBC)
      con = odbcConnect(oracle.shrimp.server,  uid=oracle.shrimp.user, pwd=oracle.shrimp.password, believeNRows=F)
        # believeNRows=F required for oracle db's
      x = sqlQuery(con, query )
      names(x) = tolower( names(x))
      save( x, file=fn, compress=T)
      return ( x )
    }
    

    if (DS %in% c("shrimp.shrcomlog", "shrimp.shrcomlog.redo") ) {
      # "test traps"
      fn = file.path( shrimp.data, "shrimp.shrcomlog.datadump.rdata" )
      dir.create( dirname( fn ), recursive=T, showWarnings=F)  
      if ( DS=="shrimp.shrcomlog" ) {
        load( fn )
        return ( x )
      }
	  
      query = paste( 
        "select *",
        "from shrcomlog "
 #       "and bcode not in ('101963','102865','101096','100200','101300','103490,',101430')" , # vessels problematic: not Mahone/St Marg
	    )
      print("WARNING --- this needs to be run from an R environment on a windows machine")
      require(RODBC)
      con = odbcConnect(oracle.shrimp.server,  uid=oracle.shrimp.user, pwd=oracle.shrimp.password, believeNRows=F) # believeNRows=F required for oracle db's
      x = sqlQuery(con, query)
      names(x) = tolower( names(x))
      
      x$yr = as.numeric( as.character( years(x$fdate) ) )
      x$julian = convert.datecodes( x$fdate, "julian" )
      x$month = ceiling(x$julian/365 *12)
      x$week = ceiling(x$julian/365 *52)
      
      # -- mobile gear
      mobile = which( x$btype %in% c( 1,3 ) )
      fixed = which( x$btype %in% c( 4 ) )
      survey = which( x$btype %in% c( 5 ) )  
      # btype = 1 ->  SF vessels based, mostly < 65ft 
      # btype = 3 ->  Gulf vessels based, > 65ft
      # btype = 4 ->  trap
      # btype = 5 ->  landings for research survey (sometimes included in TAC)
      
      x$btype.simple = NA
      x$btype.simple[ mobile] = "mobile"
      x$btype.simple[ fixed] = "fixed"
      x$btype.simple[ survey] = "survey"
      
      hrs =  trunc(x$fhours/100)
      mins = (x$fhours/100 - hrs) * 100
      
      x$fhours = hrs + mins / 60 
      
      x$lat = convert.degminsec2degdec(x$blat)
      x$lon = - convert.degminsec2degdec(x$blong)
      x$blong = NULL
      x$blat = NULL
      
      x$lon[ which(x$lon > -50) ] = NA
      x$lat[ which(x$lat < 42) ] = NA
      x$lat[ which(x$lat > 48) ] = NA
      
      debug = F
      if (debug) {
        position.good = filter.region.polygon ( x, region="scotia.fundy" )
        position.error = setdiff( 1:nrow(x), position.good) 
        print( x[ position.error ,] )
      }
      
      x$nafo = NA
      x$nafo[filter.region.polygon ( x, region="nafo.4v" )] = "4v"
      x$nafo[filter.region.polygon ( x, region="nafo.4w" )] = "4w"
      x$nafo[filter.region.polygon ( x, region="nafo.4x" )] = "4x"
      
      x$loc.small = paste("sfa", x$sfa, sep="")  
      x$loc.small[filter.region.polygon ( x, region="StMargaretsBay" )] = "StMargaretsBay"
      x$loc.small[filter.region.polygon ( x, region="MahoneBay" )] = "MahoneBay"

      save(x, file=fn, compress=T)
      return ( x )
    }
  }


