

  hydro.db = function( ip=NULL, p=NULL, DS=NULL, yr=NULL, vname=NULL, additional.data=c("groundfish", "snowcrab"), ...) {

    # manipulate temperature databases from osd, groundfish and snow crab and grid them 

    # OSD data source is 
    # http://www.meds-sdmm.dfo-mpo.gc.ca/zmp/climate/climate_e.htm
    # http://www.mar.dfo-mpo.gc.ca/science/ocean/database/data_query.html 
    ## must download manually to this directory and run gzip
    ## use choijae/Jc#00390
    ## depths: 500,500, "complete profile"   .. raw data  for the SS
    # (USER Defined -- region: jc.ss")
    basedir = project.directory("temperature", "data" )
    loc.archive = file.path( basedir, "archive", "profiles", p$spatial.domain )
    loc.basedata = file.path( basedir, "basedata", "rawdata", p$spatial.domain )
    
    dir.create( loc.basedata, recursive=T, showWarnings=F )
  
    if (exists( "init.files", p)) LoadFiles( p$init.files ) 
    if (exists( "libs", p)) RLibrary( p$libs ) 
    
    # OSD data series variables of interest
    varlist = c("DEPTH","PRESSURE","CRUISE_DATE","LATITUDE" ,"LONGITUDE" ,"TEMPERATURE" ,"SALINITY" ,"SIGMAT" ) 
 
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
          X$date = chron( dates.=X$cruise_date, format=c(dates="d/m/y"), out.format=c(dates="year-m-d")  )
          X$cruise_date = NULL
          save( X, file=fn.out, compress=T) 

        }
      }

    } 
    
    if (DS=="osd.rawdata.singleyear.redo" ) {
      for ( y in yr) {
        X = NULL
        fn.all = list.files( path=loc.archive, pattern="osd.clim.*.gz", full.names=T) 
        fn = fn.all[ grep (as.character(y), fn.all) ]
        f = read.csv( gzfile(fn), header=T, as.is=T, sep=",", na.strings="9999")
        X = f[,varlist]
         
        fn.out = file.path( loc.basedata, paste( "osd.rawdata", y, "rdata", sep=".") )
        names(X) = tolower( names(X) )
        X$date = chron( dates.=X$cruise_date, format=c(dates="d/m/y"), out.format=c(dates="year-m-d")  )
        X$cruise_date = NULL
        save( X, file=fn.out, compress=T)
      }
    } 
    
    if (DS=="osd.oneoff.singleyear.redo" ) {
        ## this is a data dump directly from Roger Petipas	
        X = NULL
        fn.all = list.files( path=loc.archive, pattern="partial.*.gz", full.names=T) 
        fn = fn.all[ grep (as.character(yr), fn.all) ]
        f = read.csv( gzfile(fn), header=T, as.is=T, sep=",", na.strings="9999")
        colnames(f) =  c( "cruiseid", "latitude", "longitude", "cruise_date", "time", 
            "pressure", "temperature", "salinity", "sigmat", "stationid" ) 
        f$depth = decibar2depth ( P=f$pressure, lat=f$latitude )
        X = f[,tolower(varlist)]

        fn.out = file.path( loc.basedata, paste( "osd.rawdata", yr, "rdata", sep=".") )
        names(X) = tolower( names(X) )
        X$date = chron( dates.=X$cruise_date, format=c(dates="d/m/y"), out.format=c(dates="year-m-d")  )
        X$cruise_date = NULL
        save( X, file=fn.out, compress=T)
    }

    if (DS=="osd.pettipas.redo" ) {
      ## this is another data dump directly from Roger Petipasfort 2010 to 2012 using MSACCESS -> text
      ## and merging here
      datadir = file.path( loc.archive, "pettipas" )
     
      for ( y in yr ) {
        print (y)
        fndata = file.path( datadir, paste( "temp_dt_", y, ".txt.xz", sep="" ) ) # xz compressed files
        fnset = file.path( datadir, paste( "temp_st_", y, ".txt.xz", sep="" ) )

        tdata = read.csv( file=xzfile(fndata), header=TRUE, stringsAsFactors=FALSE, na.strings="9999" )
        names( tdata) = c("pressure", "temperature", "salinity", "sigmat", "stationid" )

        tsets = read.csv( file=xzfile(fnset), header=TRUE, stringsAsFactors=FALSE , na.strings="9999" )
        names( tsets) = c("cruiseid", "latitude", "longitude", "cruise_date", "time", "depth_sounding", "pmax", "stationid" )

        X = merge( tdata, tsets, by="stationid", all.x=TRUE, all.y=FALSE )
        X$depth = decibar2depth ( P=X$pressure, lat=X$latitude )
        
        X = X[,tolower(varlist)]
  
        fn.out = file.path( loc.basedata, paste( "osd.rawdata", y, "rdata", sep=".") )
        names(X) = tolower( names(X) )
        
        u = matrix( unlist(strsplit( X$cruise_date, split=" ")), nrow=nrow(X), byrow=TRUE )

        X$date = chron( dates.=u[,1], format=c(dates="y-m-d"), out.format=c(dates="year-m-d")  )
        X$cruise_date = NULL
       
        X$temperature = as.numeric( X$temperature ) 
        X$salinity= as.numeric(X$salinity)
        X$sigmat = as.numeric(X$sigmat)
  
        save( X, file=fn.out, compress=T)
      }
     
    }


    # ----------------  

    if (DS %in% c( "profiles.annual.redo", "profiles.annual" ) ) {
      # read in annual depth profiles then extract bottom temperatures 
      
      if (p$spatial.domain %in% c("SSE", "snowcrab")  ) p$spatial.domain="canada.east"  ## no point in having these as they are small subsets

      basedir = project.directory("temperature", "data" )
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

      for (iy in ip) {
        yt = p$runs[iy, "yrs"]
        Y = hydro.db( DS="osd.rawdata", yr=yt, p=p )
          if ( is.null(Y) ) {
            Y = hydro.db( DS="osd.rawdata", yr=2000, p=p ) [1,]  # dummy entry using year=2000
            Y$id =  "dummy"
            Y$yr =  yt
            Y$dayno = 1
            Y$weekno = 1
            Y$depth = -1
            Y$oxyml = NA
  
          } else {

            Y$id =  paste( Y$longitude, Y$latitude, Y$dayno, sep="~" )
            Y$yr =  as.numeric( as.character( years( Y$date ) ) )
            Y$dayno = convert.datecodes(Y$date, "julian") 
            Y$weekno = ceiling ( Y$dayno / 365 * 52 )
            Y$depth = decibar2depth ( P=Y$pressure, lat=Y$latitude )
            Y$oxyml = NA
            
            # next should not be necessary .. but just in case the osd data types get altered  
            Y$temperature = as.numeric(Y$temperature ) 
            Y$salinity= as.numeric(Y$salinity)
            Y$sigmat = as.numeric(Y$sigmat)
          }
        
        Y$pressure = NULL

        if ("groundfish" %in% additional.data ) {
          gf = grdfish[ which( grdfish$yr == yt ) , ]
					if (nrow(gf) > 0) {
						gf$sigmat = NA
						names(gf) = c( "id", "depth", "temperature", "salinity", "oxyml", "longitude", "latitude", "yr", "weekno", "dayno", "date", "sigmat"  )
						Y = rbind( Y, gf[, names(Y)] )
					}
				}

        if ("snowcrab" %in% additional.data ) {
	        
          minilog = minilog.db( DS="basedata", Y=yt )

          if (! is.null( nrow( minilog ) ) ) {
            minilog = merge( minilog, set, by="minilog_uid", all.x=TRUE, all.y=FALSE ) 
            minilog$id = minilog$minilog_uid 
            minilog$yr = as.numeric( as.character( years( minilog$chron) ) )
            minilog$dayno = convert.datecodes(minilog$chron, "julian") 
            minilog$weekno = ceiling ( minilog$dayno / 365 * 52 )
            minilog$date = minilog$chron
            Y = rbind( Y, minilog[, names(Y) ] )
          }

          seabird = seabird.db( DS="basedata", Y=yt )
					if ( !is.null( nrow( seabird ) ) ) {
            seabird = merge( seabird, set, by="seabird_uid", all.x=TRUE, all.y=FALSE ) 
            seabird$id = seabird$seabird_uid
            seabird$yr = as.numeric( as.character( years( seabird$chron) ) )
            seabird$dayno = convert.datecodes( seabird$chron, "julian") 
            seabird$weekno = ceiling ( seabird$dayno / 365 * 52 )
            seabird$date = seabird$chron
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

      basedir = project.directory("temperature", "data" )
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
        # Bottom temps
        if (is.null(Y)) next()
				Y$id =  paste( round(Y$longitude,2), round(Y$latitude,2), Y$dayno, sep="~" )
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
      
      basedir = project.directory("temperature", "data" )
      loc.gridded = file.path( basedir, "basedata", "gridded", "bottom", p$spatial.domain )
      dir.create( loc.gridded, recursive=T, showWarnings=F )
      
      if (DS == "bottom.gridded.all" ) {
        if (is.null(yr)) yr=p$tyears # defaults to tyears if no yr specified 
        O = NULL
        fn = file.path( loc.gridded, paste( "bottom.allyears", "rdata", sep="." ) )
        if (file.exists(fn)) { 
          load (fn) 
        } else {
          for (y in p$tyears ) {
            On = hydro.db(p=p, DS="bottom.gridded", yr=y) 
            if ( is.null( On) ) next()
            O = rbind( O, On )
          }
          save( O, file=fn, compress=TRUE )
        }
        O = O[ which( O$yr %in% yr) , ]
        return(O)
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
          tp$date = NULL
					# tp$depth = NULL
         
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
          tp$st = paste( tp$weekno, tp$plon, tp$plat ) 
        
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
  

