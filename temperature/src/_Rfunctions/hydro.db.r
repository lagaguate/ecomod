

  hydro.db = function( ip=NULL, p=NULL, DS=NULL, yr=NULL, vname=NULL, additional.data=c("groundfish", "snowcrab"), ...) {

    # manipulate temperature databases from osd, groundfish and snow crab and grid them 
    # OSD data source is 
    # http://www.meds-sdmm.dfo-mpo.gc.ca/zmp/climate/climate_e.htm
    # http://www.mar.dfo-mpo.gc.ca/science/ocean/database/data_query.html 
    ## must download manually to this directory and run gzip
    ## use choijae/Jc#00390
    ## depths: 500,500, "complete profile"   .. raw data  for the SS
    # (USER Defined -- region: jc.ss")
    basedir = project.datadirectory("temperature", "data" )
    loc.archive = file.path( basedir, "archive", "profiles", p$spatial.domain )
    loc.basedata = file.path( basedir, "basedata", "rawdata", p$spatial.domain )
    dir.create( loc.basedata, recursive=T, showWarnings=F )
    if (exists( "init.files", p)) LoadFiles( p$init.files ) 
    if (exists( "libs", p)) RLibrary( p$libs ) 
    # OSD data series variables of interest
    

    if ( DS == "osd.rawdata" ) {
      # simple loading of annual data files
      out = NULL
      for ( y in yr ) {
        print (y)
        fn = file.path( loc.basedata, paste( "osd.rawdata", y, "rdata", sep=".") )
        if (file.exists ( fn ) ) {
          load(fn)
          out = rbind( out, X )
        }
      }
      return ( out )
    }

				
    if ( DS=="osd.rawdata.allfiles.redo" ) {
      fn.all = list.files( path=loc.archive, pattern="osd.clim.*.gz", full.names=T) 
      X = NULL
      varlist = c("DEPTH","PRESSURE","CRUISE_DATE","LATITUDE" ,"LONGITUDE" ,"TEMPERATURE","SALINITY" ,"SIGMAT" ) 
      varstosave = c( "depth", "pressure", "latitude" ,"longitude" ,"temperature" ,"salinity" ,"sigmat", "date" )
      for (fn in fn.all) {
        f = read.csv( gzfile(fn), header=T, as.is=T, sep=",", na.strings="9999")
        f = f[,varlist] 
        fyears = as.numeric( matrix( unlist( strsplit( f$CRUISE_DATE, "/" ) ), ncol=3, byrow=T) [,3] )
        years = sort( unique( fyears ))
        for (yrs in years) {
          fn.out = file.path( loc.basedata,  paste( "osd.rawdata", yrs, "rdata", sep=".") )
          print( paste(yrs, ":", fn.out) )
          X = f[ which( fyears == yrs) ,]
          names(X) = tolower( names(X) )
          X$date = lubridate::dmy( X$cruise_date )
          X = X[ , varstosave ]
          save( X, file=fn.out, compress=T) 
        }
      }

    } 
    
    if (DS=="osd.rawdata.singleyear.redo" ) {
      varlist = c("DEPTH","PRESSURE","CRUISE_DATE","LATITUDE" ,"LONGITUDE" ,"TEMPERATURE","SALINITY" ,"SIGMAT" ) 
      varstosave = c( "depth", "pressure", "latitude" ,"longitude" ,"temperature" ,"salinity" ,"sigmat", "date" )
      for ( y in yr) {
        X = NULL
        fn.all = list.files( path=loc.archive, pattern="osd.clim.*.gz", full.names=T) 
        fn = fn.all[ grep (as.character(y), fn.all) ]
        f = read.csv( gzfile(fn), header=T, as.is=T, sep=",", na.strings="9999")
        X = f[,varlist]
        fn.out = file.path( loc.basedata, paste( "osd.rawdata", y, "rdata", sep=".") )
        names(X) = tolower( names(X) )
        X$date = lubridate::ymd( X$cruise_date )
        X= X[, varstosave ]
        save( X, file=fn.out, compress=T)
      }
    } 
 
    # ----------------------

    if (DS=="osd.initial" ) {
      ## this is a data dump directly from Roger Pettipas for 2008 to 2015 
      varstosave = c( "depth", "pressure", "latitude" ,"longitude" ,"temperature" ,"salinity" ,"sigmat", "date" )
      fndata = file.path( loc.archive, "Data_2008-2014.csv.xz" ) 
      XX = read.csv( file=xzfile(fndata), header=TRUE, stringsAsFactors=FALSE, na.strings="9999" )
      names(XX) = tolower( names(XX) )
      XX$depth = decibar2depth ( P=XX$pressure, lat=XX$latitude )
      if (!exists( "sigmat", XX))  XX$sigmat = XX$sigma.t  # naming is variable
      XX$date_string = paste( XX$year, XX$month, XX$day, sep="-" )
      XX$date = lubridate::ymd( XX$date_string ) 
      yrs = sort( unique( XX$year) )
      for ( y in yrs ) {
        print (y)
        fn.out = file.path( loc.basedata, paste( "osd.rawdata", y, "rdata", sep=".") )
        ii = which ( XX$year == y )
        if (length(ii) > 1) {
          X= XX[ ii, varstosave ]
          save( X, file=fn.out, compress=T)
        }
      }
    }

 
    # ----------------------


    if (DS=="osd.current" ) {
      ## this is a data dump directly from Roger Pettipas for 2015 and on 
      varstosave = c( "depth", "pressure", "latitude" ,"longitude" ,"temperature" ,"salinity" ,"sigmat", "date" )
      for ( y in yr ) {
        print (y)
        fndata = file.path( loc.archive, paste( "Data_", y, ".csv.xz", sep="" ) ) 
        fn.out = file.path( loc.basedata, paste( "osd.rawdata", y, "rdata", sep=".") )
        X = read.csv( file=xzfile(fndata), header=TRUE, stringsAsFactors=FALSE, na.strings="9999" )
        names(X) = tolower( names(X) )
        X$depth = decibar2depth ( P=X$pressure, lat=X$latitude )
        if (!exists( "sigmat", X))  X$sigmat = X$sigma.t  # naming is variable
        X$date_string = paste( X$year, X$month, X$day, sep="-" )
        X$date = lubridate::ymd( X$date_string )
        X= X[, varstosave ]
        save( X, file=fn.out, compress=T)
      }
    }


    # ----------------  

    if (DS %in% c( "profiles.annual.redo", "profiles.annual" ) ) {
      # read in annual depth profiles then extract bottom temperatures 
      
      if (p$spatial.domain %in% c("SSE", "snowcrab")  ) p$spatial.domain="canada.east"  ## no point in having these as they are small subsets

      basedir = project.datadirectory("temperature", "data" )
      loc.profile = file.path( basedir, "basedata", "profiles", p$spatial.domain )
      dir.create( loc.profile, recursive=T, showWarnings=F )
 
      if (DS=="profiles.annual") {
        fn = file.path(  loc.profile, paste("depthprofiles", yr, "rdata", sep="."))
        Y = NULL
				if (file.exists( fn) ) load (fn )
        return(Y)
      }
 
      ####### "ip" is the first parameter expected when run in parallel mode .. do not move this one
 
        if ( is.null(ip)) {
          if( exists( "nruns", p ) ) {
            ip = 1:p$nruns
          } else { 
            if ( !is.null(yr)) {
              # if only selected years being re-run
              ip = 1:length(yr)
              p$runs = data.frame(yrs = yr)
            } else {
              ip = 1:length(p$tyears)
              p$runs = data.frame(yrs = p$tyears)
            }
          }
        }

      # bring in snow crab, groundfish and OSD data ...
      
      loadfunctions( "snowcrab")  # only need the functions not the whole environment
      
      set = snowcrab.db( DS="setInitial" )
      mlu = minilog.db( DS="set.minilog.lookuptable" )
      slu = seabird.db( DS="set.seabird.lookuptable" )
      set = merge( set, mlu, by= c("trip", "set"), all.x=TRUE, all.y=FALSE )
      set = merge( set, slu, by= c("trip", "set"), all.x=TRUE, all.y=FALSE )
      set$longitude =set$lon
      set$latitude = set$lat
      set$oxyml = NA
      set$salinity = NA
      set$sigmat = NA
      
      set = set[ ,c("minilog_uid", "seabird_uid", "longitude", "latitude", "oxyml", "salinity", "sigmat" ) ]
			
      loadfunctions( "groundfish")
      grdfish = groundfish.db( "gshyd.georef" )

      Ydummy = hydro.db( DS="osd.rawdata", yr=2000, p=p ) [1,]  # dummy entry using year=2000
      Ydummy$yr = NA
      Ydummy$dyear = 0.5
      Ydummy$id =  "dummy"
      Ydummy$depth = -1
      Ydummy$oxyml = NA
      
      for (iy in ip) {
        yt = p$runs[iy, "yrs"]
        
        Y = hydro.db( DS="osd.rawdata", yr=yt, p=p )
          if ( is.null(Y) ) {
            Y = Ydummy
            Y$yr = yt
          } else {
            Y$yr = yt
            Y$dyear = lubridate::decimal_date( Y$date ) - Y$yr
            Yid = cut( Y$dyear, breaks=p$dyears, include.lowest=T, right=F, ordered_result=TRUE )
            Y$id =  paste( round(Y$longitude,2), round(Y$latitude,2), Yid , sep="~" )
            Y$depth = decibar2depth ( P=Y$pressure, lat=Y$latitude )
            Y$oxyml = NA
            # next should not be necessary .. but just in case the osd data types get altered  
            Y$temperature = as.numeric(Y$temperature ) 
            Y$salinity= as.numeric(Y$salinity)
            Y$sigmat = as.numeric(Y$sigmat)
          }
        
        Y$pressure = NULL
        
        
        if ("groundfish" %in% additional.data ) {
          gfkeep = c( "id", "sdepth", "temp", "sal", "oxyml", "lon", "lat", "yr", "date")  
          gf = grdfish[ which( grdfish$yr == yt ) , gfkeep ]
					if (nrow(gf) > 0) {
						gf$sigmat = NA
						gf$date = as.POSIXct(gf$date, origin=lubridate::origin) 
            gf$dyear = lubridate::decimal_date( gf$date ) - gf$yr
            names(gf) = c( "id", "depth", "temperature", "salinity", "oxyml", "longitude", "latitude", "yr", "date", "dyear", "sigmat"  )
            Y = rbind( Y, gf[, names(Y)] )
					}
				}

        if ("snowcrab" %in% additional.data ) {
	        
          minilog = minilog.db( DS="basedata", Y=yt )

          if (! is.null( nrow( minilog ) ) ) {
            minilog = merge( minilog, set, by="minilog_uid", all.x=TRUE, all.y=FALSE ) 
            minilog$id = minilog$minilog_uid 
            minilog$date = as.POSIXct(minilog$chron, origin=lubridate::origin)
            minilog$yr = yt
            minilog$dyear = lubridate::decimal_date( minilog$date ) - minilog$yr
            Y = rbind( Y, minilog[, names(Y) ] )
          }

          seabird = seabird.db( DS="basedata", Y=yt )
					if ( !is.null( nrow( seabird ) ) ) {
            seabird = merge( seabird, set, by="seabird_uid", all.x=TRUE, all.y=FALSE ) 
            seabird$id = seabird$seabird_uid
            seabird$yr = yt
            seabird$date = as.POSIXct(seabird$chron, origin=lubridate::origin)
            seabird$dyear = lubridate::decimal_date( seabird$date ) - seabird$yr
            seabird$oxyml = NA
            Y = rbind( Y, seabird[, names(Y) ] )
          }
				
        }
        
        oo = which( Y$id == "dummy" )
        if (length(oo) > 0 ) Y = Y[ -oo, ]

        if ( is.null( nrow(Y) ) ) next()
        if ( nrow(Y) < 5 ) next()

        if ( is.null(Y) ) next()

        iiY = which(duplicated(Y))
        if (length(iiY)>0) Y = Y [ -iiY, ]

        bad = which( Y$temperature < -5 | Y$temperature > 30 ) 
        if (length(bad)>0) Y=Y[-bad,]
        
        fn = file.path( loc.profile, paste("depthprofiles", yt, "rdata", sep="."))
        print( fn )
        save( Y, file=fn, compress=T )
      }

      return ("Completed")
    }


    # ----------------  

    if (DS %in% c( "bottom.annual", "bottom.annual.redo" ) ) {
      # extract bottom temperatures
     
      if (p$spatial.domain %in% c("SSE", "snowcrab")  ) p$spatial.domain="canada.east"  ## no point in having these as they are small subsets

      basedir = project.datadirectory("temperature", "data" )
      loc.bottom = file.path( basedir, "basedata", "bottom", p$spatial.domain )
      dir.create( loc.bottom, recursive=T, showWarnings=F )
 
      if (DS=="bottom.annual") {
        fn = file.path( loc.bottom, paste("bottom", yr, "rdata", sep="."))
        Z = NULL
				if (file.exists(fn) ) load (fn )
        return(Z)
      }

        if ( is.null(ip)) {
          if( exists( "nruns", p ) ) {
            ip = 1:p$nruns
          } else { 
            if ( !is.null(yr)) {
              # if only selected years being re-run
              ip = 1:length(yr)
              p$runs = data.frame(yrs = yr)
            } else {
              ip = 1:length(p$tyears)
              p$runs = data.frame(yrs = p$tyears)
            }
          }
        }


      for (iy in ip) {
        yt = p$runs[iy, "yrs"]
        Y = hydro.db( DS="profiles.annual", yr=yt, p=p )
        if (is.null(Y)) next()
        igood = which( Y$temperature >= -3 & Y$temperature <= 25 )  ## 25 is a bit high but in case some shallow data 
        Y = Y[igood, ]
 
        # Bottom temps
				
        Yid = cut( Y$dyear, breaks=p$dyears, include.lowest=T, right=F, ordered_result=TRUE )
        Y$id =  paste( round(Y$longitude,2), round(Y$latitude,2), Yid, sep="~" )
        ids =  sort( unique( Y$id ) )
        res = copy.data.structure( Y)   

        for (i in ids ) {
          Z = Y[ which( Y$id == i ), ]
          jj = which( is.finite( Z$depth ) )
          if ( length(jj) < 3 ) next() 
          zmax = max( Z$depth, na.rm=T ) - 10  # accept any depth within 10 m of the maximum depth
          kk =  which( Z$depth >= zmax ) 
          R = Z[ which.max( Z$depth ) , ]
          R$temperature = median( Z$temperature[kk] , na.rm=T ) 
          R$salinity = median( Z$salinity[kk] , na.rm=T )
          R$sigmat = median( Z$sigmat[kk] , na.rm=T )
          R$oxyml = median( Z$oxyml[kk] , na.rm=T )
          res = rbind( res, R )
        }
        
        Z = res
        fn = file.path( loc.bottom, paste("bottom", yt, "rdata", sep="."))
				print (fn)
        save( Z, file=fn, compress=T)
      }
      return ("Completed")

    }


    # -----------------

    if (DS %in% c( "bottom.gridded", "bottom.gridded.redo" , "bottom.gridded.all" )){
      # this is stored locally and not as an archive for speed and flexibility
      
      basedir = project.datadirectory("temperature", "data" )
      loc.gridded = file.path( basedir, "basedata", "gridded", "bottom", p$spatial.domain )
      dir.create( loc.gridded, recursive=T, showWarnings=F )
      
      O = NULL
      fnall = file.path( loc.gridded, paste( "bottom.allyears", "rdata", sep="." ) )

      if (DS == "bottom.gridded.all" ) {
        if (is.null(yr)) yr=p$tyears # defaults to tyears if no yr specified 
        if (file.exists(fnall)) { 
          load (fnall) 
        } 
        O = O[ which( O$yr %in% yr) , ]
        return(O)
      }
   
      if (DS == "bottom.gridded.all.redo" ) {
        if (is.null(yr)) yr=p$tyears # defaults to tyears if no yr specified 
        for (y in p$tyears ) {
            On = hydro.db(p=p, DS="bottom.gridded", yr=y) 
            if ( is.null( On) ) next()
            O = rbind( O, On )
          }
        save( O, file=fnall, compress=TRUE )
        O = O[ which( O$yr %in% yr) , ]
        return( fnall )
      }


      if (DS == "bottom.gridded" ) {
				fng = file.path(  loc.gridded, paste( "bottom", yr, "rdata", sep="." ) )
        tp = NULL
				if (file.exists(fng) ) load(fng)
        return ( tp )
      }
       
  
      if (DS == "bottom.gridded.redo" ) {
          
        if ( is.null(ip)) {
          if( exists( "nruns", p ) ) {
            ip = 1:p$nruns
          } else { 
            if ( !is.null(yr)) {
              # if only selected years being re-run
              ip = 1:length(yr)
              p$runs = data.frame(yrs = yr)
            } else {
              ip = 1:length(p$tyears)
              p$runs = data.frame(yrs = p$tyears)
            }
          }
        }

        for (iip in ip) {
          y = p$runs[iip, "yrs"]
          fn = file.path( loc.gridded , paste( "bottom", y, "rdata", sep="." ) )
          print(fn)
          tp = NULL
          pcanada.east = p
          pcanada.east$spatial.domain="canada.east" 
          # must "force" the following to read data from a larger spatial extent 
          # (as this is all that is currently stored) .. can used SSE specific 
          # but that increases storgage and data duplication .. no need
          tp = hydro.db( p=pcanada.east, DS="bottom.annual", yr=y )
         
          if (is.null( tp) ) next()
					tp = rename.df( tp, "longitude", "lon")
          tp = rename.df( tp, "latitude", "lat")
          tp = rename.df( tp, "temperature", "t")
          tp = rename.df( tp, "depth", "z")

          tp$date = as.Date( tp$date ) # strip out time of day information
          tp$dyear = lubridate::decimal_date( tp$date ) - tp$yr
					# tp$depth = NULL
        
          igood = which( tp$t >= -3 & tp$t <= 25 )  ## 25 is a bit high but in case some shallow data 
          tp = tp[igood, ]
 
          igood = which( tp$lon >= p$corners$lon[1] & tp$lon <= p$corners$lon[2] 
              &  tp$lat >= p$corners$lat[1] & tp$lat <= p$corners$lat[2] )
          tp = tp[igood, ]

          tp = lonlat2planar( tp, proj.type=p$internal.projection )
          
					tp$lon = grid.internal( tp$lon, p$lons )
          tp$lat = grid.internal( tp$lat, p$lats )
        
					tp$plon = grid.internal( tp$plon, p$plons )
          tp$plat = grid.internal( tp$plat, p$plats )
	
					tp = tp[ which( is.finite( tp$lon + tp$lat + tp$plon + tp$plat ) ) , ]
          ## ensure that inside each grid/time point 
          ## that there is only one point estimate .. taking medians
          vars = c("z", "t", "salinity", "sigmat", "oxyml")
          tp$st = paste( tp$mon, tp$plon, tp$plat ) 
        
          o = which( ( duplicated( tp$st )) )
          if (length(o)>0) { 
            dupids = unique( tp$st[o] )
            for ( dd in dupids ) {
              e = which( tp$st == dd )
              keep = e[1]
              drop = e[-1]
              for (v in vars) tp[keep, v] = median( tp[e,v], na.rm=TRUE )
              tp$st[drop] = NA  # flag for deletion
            }
            tp = tp[ -which( is.na( tp$st)) ,]  
          }
          tp$st = NULL

					save( tp, file=fn, compress=T)
        }
      }
      return ( "Completed rebuild"  )
    }
    
}
  

