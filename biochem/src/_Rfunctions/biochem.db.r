
## http://www.meds-sdmm.dfo-mpo.gc.ca/biochemQuery/authenticate.do?errors=yes

## Login : choij
## Pwd : BIOjc!290

# Biochem data analysis  .. focus on bottom oxygen
 
  biochem.db = function(DS="", ss=NULL) {
      
    biochem.dir = file.path( "/home", "jae", "ecomod", "biochem") 
    
    if (DS %in% c("scotian.shelf.redo", "scotian.shelf") ){

      fn = file.path( biochem.dir, "ss.rdata" )

      if (DS=="scotian.shelf") {
        load( fn)
        return(ss)
      }

      if (DS=="scotian.shelf.redo") {
        ess = read.table(file= file.path(biochem.dir, "ess.dat"), sep=",",header=T)
        ess$region = "4VW"
        wss = read.table(file= file.path(biochem.dir, "wss.dat"), sep=",",header=T)
        wss$region = "4X"

        ss = rbind(ess, wss)
        rm(ess, wss)

        names(ss) =  tolower( names(ss) )
        namelist = data.frame( matrix( c(
          "collector_station_name", "station",
          "data_center_code",       "source",
          "start_lat",              "lat",
          "start_lon",              "lon",
          "start_date",              "date"
          ), ncol=2, byrow=T) )
        namelistnames = names(namelist) = c("old", "new")
        namelist = factor2character( namelist, namelistnames)

        for (i in 1:nrow(namelist)) names(ss)[which(names(ss)==namelist$old[i])] = namelist$new[i]

        ss$Z = rowMeans(ss[,c("start_depth","end_depth")], na.rm=T)

        require(chron)
        ss$chron = chron( dates.=as.character(ss$date), format=c(dates="day-mon-year"), out.format=c(dates="year-m-d") )

        ss$id = paste( ss$lon, ss$lat, as.character(ss$chron), sep="~" )
        ss$id2 = paste( ss$lon, ss$lat, as.character(ss$chron), ss$Z, sep="~" )
        ss = ss[ !duplicated(ss$id2) , ]

        tokeep = c( 
          "id", "region", "lon", "lat", "Z", "gear_model", "salinity", "salinity_datatype",
          "temperature", "temperature_datatype", "o2", "o2_datatype", "sigma_t", 
          "oxygen_saturation", "chron" )
        ss = ss[, tokeep ]

        save(ss, file=fn, compress=T)
        return ( fn )
      }
    } 

    if (DS %in% c("sse.bottom", "sse.bottom.redo" ) ) {
      
      fn = file.path(  biochem.dir, "ss.bottom.rdata" )

      if (DS=="sse.bottom") {
        load( fn)
        return(ss)
      }

      if (DS == "sse.bottom.redo" ) {
        
        ss = biochem.db( DS="scotian.shelf" )

        ids = sort( unique( ss$id ) )
        s = NULL
        t = which( is.finite(ss$Z) )
        for (i in ids) {
          q = intersect( which( ss$id==i ), t )
          r = which.max( ss$Z[q] )
          s = c(s, q[-r])
        }
        ss = ss[-s,]
        ss$julian = convert.datecodes( ss$chron, "julian" )
        ss$yr =  convert.datecodes( ss$chron, "year" )

        # select shelf data:
        # ss = ss[ which(ss$Z < 600 & ss$Z > 75) , ]
        # ss = ss[ which(ss$yr >= 1960), ]

        save(ss, file=fn, compress=T)
        return (fn )
      }
  
    }

    if ( DS=="visual.check.of spatial.spead.of data") {
      # visual check of the spatial spread of the data
      if (is.null(ss)) ss = biochem.db( DS="scotian.shelf" )
      x11()
      yrs = sort( unique( ss$yr) )
      for (y in yrs) {
        ii = which( ss$yr==y & is.finite(ss$oxygen_saturation ))
        plot( ss$lon, ss$lat, type="n", main=y)
        points(ss$lon[ii], ss$lat[ii])
        print(y)
        pause(5)
      }

      # results:
      # good overall spatial coverage in 1970, 1999-2005
      # good only for 4x : 33 60 66 68 71 73 
        good.4vw = c(1970, 1999:2005)
        good.4x  = c(1933, 1960, 1966, 1968, 1970, 1971, 1973, 1999:2005)

    }

    if (DS=="oxygen.annual.old") {
      if (is.null(ss)) ss = biochem.db( DS="scotian.shelf" )
      oxy = stats.by.factors(ss$oxygen_saturation ,  list(yr=ss$yr, region=ss$region) )
      oxy$yr = as.numeric( as.character (oxy$yr) ) 
      oxy = oxy[ order(oxy$region, oxy$yr) ,]
      return (oxy)
    }

    if (DS=="oxygen.annual") {
      if (is.null(ss)) ss = biochem.db( DS="scotian.shelf" )
      oxy = NULL
      yrs = sort( unique( ss$yr) )
      regions = c("4VW", "4X")
      for (r in regions) {
      for (y in yrs) {
        tt = which( ss$yr==y & ss$region==r)
        
        if (y==1970) { # selectively remove one area that was heavily sampled relative the rest of the shelf
          tt = intersect( tt, which(ss$lat < 45.4) ) 
        }

        if (length( tt) < 10) next
        mean = mean( ss$oxygen_saturation [ tt ], na.rm=T, trim=0.1 )
        sd = sd(  ss$oxygen_saturation [ tt ], na.rm=T )
        n = length( which( is.finite(   ss$oxygen_saturation [ tt ] ) ) )
        se = sd / sqrt(n-1)
        ub = mean + 4*se
        lb = mean - 4*se
        
        rob = intersect( tt, which( ss$oxygen_saturation <=ub & ss$oxygen_saturation >= lb) )
        if (length( rob) < 10) next
        rmean = mean( ss$oxygen_saturation [ rob ], na.rm=T, trim=0.1 )
        rsd = sd( ss$oxygen_saturation [ rob ], na.rm=T )
        rn = length( which( is.finite( ss$oxygen_saturation [ rob ] ) ) )
        rse = rsd / sqrt(rn - 1)

        oxy = rbind( oxy, data.frame(region=r, yr=y, mean=rmean, se=rse, n=rn ) )

      }}
      return (oxy )
    }

    if ( DS == "ess" ) {
      good.4vw = c(1970, 1999:2005)
      oxy = biochem.db( DS="oxygen.annual", ss=biochem.db( DS="sse.bottom" ) )
      ess = which(oxy$region=="4VW" & oxy$yr %in% good.4vw )
      x11()
      xval = oxy$yr[ess]
      yval = oxy$mean[ess]
      weight = oxy$n[ess]
      plot(xval, yval)
      lss = predict( loess( yval ~ xval, span=0.5, degree=1), weight=weight, data.frame(xval=xval), se=T )
      lines( xval, lss$fit, col="gray", lty="solid", lwd=4 )
      return (ess)
    }


    if ( DS == "wss" ) {
      oxy = biochem.db( DS="oxygen.annual", ss=biochem.db( DS="sse.bottom" ) )
      good.4x  = c(1933, 1960, 1966, 1968, 1970, 1971, 1973, 1999:2005)
      wss = which(oxy$region=="4X" & oxy$yr %in% good.4x)
      x11()
      xval = oxy$yr[wss]
      yval = oxy$mean[wss]
      weight = oxy$n[wss]
      plot(xval, yval)
      lss = predict( loess( yval ~ xval, span=0.5, degree=1), weight=weight, data.frame(xval=xval), se=T )
      lines( xval, lss$fit, col="gray", lty="solid", lwd=4 )
      return (ess)
    }



}

        

