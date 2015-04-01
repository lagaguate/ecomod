
  osd.data = function( source, osd.dir=file.path( project.datadirectory("indicators"), "data", "osd" ), ... ) {

    if ( source %in% c("seaice", "seaice.refresh")) {
      
      # http://www.meds-sdmm.dfo-mpo.gc.ca/zmp/climate/IceCoverage_e.htm

      loc = "http://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/zmp/climate/IceCover/odf_data_files/SCOTSHELF_Ice_Area.odf"
      fn = file.path( osd.dir, basename(loc) )
      if ( source=="seaice.refresh") download.file( url=loc, destfile=fn )


      test = readLines(fn, n=200)
      nheaderlines = grep( "-- DATA --", test, ignore.case=T)
      data = read.table( fn, skip=nheaderlines, as.is=T)

      names(data) = c("date", "sa.km2")
      x = unlist (strsplit( data$date, " ", fixed=T ))
      x = matrix(x, ncol=2, byrow=T)
      data$date = chron( dates.=x[,1], format=c(dates="d-mon-year"), out.format=c(dates="year-m-d")   )
    
      return (data )
    }

    if ( source %in% c("icebergs", "icebergs.refresh")) {
       
       # http://www.meds-sdmm.dfo-mpo.gc.ca/zmp/climate/icebergs_e.htm
       # number per year from October to September

      loc = "http://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/zmp/climate/Icebergs/odf_data_files/ICEBERGS_South_of_48_Latitude.odf"
      fn = file.path( osd.dir, basename(loc) )
      if ( source=="icebergs.refresh") download.file( url=loc, destfile=fn )

      test = readLines(fn, n=200)
      nheaderlines = grep( "-- DATA --", test, ignore.case=T)
      data = read.table( fn, skip=nheaderlines, as.is=T)

      names(data) = c("date", "n.icebergs")
      x = unlist (strsplit( data$date, " ", fixed=T ))
      x = matrix(x, ncol=2, byrow=T)
      data$date = chron( dates.=x[,1], format=c(dates="d-mon-year"), out.format=c(dates="year-m-d")   )
    
      return (data )
    }

    if ( source %in% c("freshwater.runoff.quebec", "freshwater.runoff.quebec.refresh")) {
   
      # http://www.meds-sdmm.dfo-mpo.gc.ca/zmp/climate/DrainageBasins_e.asp?id=rivsum
      loc = "http://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/zmp/Climate/DrainageBasins/odf_data_files/rivsum.odf"
      fn = file.path( osd.dir, basename(loc) )
      if ( source=="freshwater.runoff.quebec.refresh") download.file( url=loc, destfile=fn )

      test = readLines(fn, n=200)
      nheaderlines = grep( "-- DATA --", test, ignore.case=T)
      data = read.table( fn, skip=nheaderlines, as.is=T)

      names(data) = c("date", "discharge.m3.sec")
      x = unlist (strsplit( data$date, " ", fixed=T ))
      x = matrix(x, ncol=2, byrow=T)
      data$date = chron( dates.=x[,1], format=c(dates="d-mon-year"), out.format=c(dates="year-m-d")   )
    
      return (data )
    }
    
    if ( source %in% c("freshwater.runoff.into.hudsonbay", "freshwater.runoff.into.hudsonbay.refresh")) {
     
      # http://www.meds-sdmm.dfo-mpo.gc.ca/zmp/climate/DrainageBasins_e.asp?id=hudson_bay
      loc = "http://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/zmp/Climate/DrainageBasins/odf_data_files/hudson_bay.odf"
      fn = file.path( osd.dir, basename(loc) )
      if ( source=="freshwater.runoff.into.hudsonbay.refresh") download.file( url=loc, destfile=fn )

      test = readLines(fn, n=200)
      nheaderlines = grep( "-- DATA --", test, ignore.case=T)
      data = read.table( fn, skip=nheaderlines, as.is=T)

      names(data) = c("date", "discharge.m3.sec")
      x = unlist (strsplit( data$date, " ", fixed=T ))
      x = matrix(x, ncol=2, byrow=T)
      data$date = chron( dates.=x[,1], format=c(dates="d-mon-year"), out.format=c(dates="year-m-d")   )
    
      return (data )
    }

    if ( source %in% c("freshwater.discharge.stjohn", "freshwater.discharge.stjohn.refresh")) {
      
      loc = "http://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/zmp/Climate/DrainageBasins/odf_data_files/stjohn_river.odf"
      fn = file.path( osd.dir, basename(loc) )
      if ( source=="freshwater.discharge.stjohn.refresh") download.file( url=loc, destfile=fn )

      test = readLines(fn, n=200)
      nheaderlines = grep( "-- DATA --", test, ignore.case=T)
      data = read.table( fn, skip=nheaderlines, as.is=T)

      names(data) = c("date", "discharge.m3.sec")
      x = unlist (strsplit( data$date, " ", fixed=T ))
      x = matrix(x, ncol=2, byrow=T)
      data$date = chron( dates.=x[,1], format=c(dates="d-mon-year"), out.format=c(dates="year-m-d")   )
    
      return (data )
    }


    
    if ( source %in% c("t.sydney.ns", "t.sydney.ns.refresh")) {
      
      # http://www.meds-sdmm.dfo-mpo.gc.ca/zmp/climate/AirTemp_f.asp?stn=Sydney
      loc = "http://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/zmp/climate/AirTemp/odf_data_files/Sydney_Monthly_Air_Temp.odf"
      fn = file.path( osd.dir, basename(loc) )
      if ( source=="t.sydney.ns.refresh") download.file( url=loc, destfile=fn )

      test = readLines(fn, n=200)
      nheaderlines = grep( "-- DATA --", test, ignore.case=T)
      data = read.table( fn, skip=nheaderlines, as.is=T)

      names(data) = c("date", "t.sydney.C")
      x = unlist (strsplit( data$date, " ", fixed=T ))
      x = matrix(x, ncol=2, byrow=T)
      data$date = chron( dates.=x[,1], format=c(dates="d-mon-year"), out.format=c(dates="year-m-d")   )
    
      return (data )
    }

   
    if ( source %in% c("t.sable.ns", "t.sable.ns.refresh")) {
      
      # http://www.meds-sdmm.dfo-mpo.gc.ca/zmp/climate/AirTemp_f.asp?stn=Sableisl
      loc = "http://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/zmp/climate/AirTemp/odf_data_files/Sableisl_Monthly_Air_Temp.odf"
      fn = file.path( osd.dir, basename(loc) )
      if ( source=="t.sable.ns.refresh") download.file( url=loc, destfile=fn )

      test = readLines(fn, n=200)
      nheaderlines = grep( "-- DATA --", test, ignore.case=T)
      data = read.table( fn, skip=nheaderlines, as.is=T)

      names(data) = c("date", "t.sable.C")
      x = unlist (strsplit( data$date, " ", fixed=T ))
      x = matrix(x, ncol=2, byrow=T)
      data$date = chron( dates.=x[,1], format=c(dates="d-mon-year"), out.format=c(dates="year-m-d")   )
    
      return (data )
    }

 
    if ( source %in% c("t.halifax.ns", "t.halifax.ns.refresh")) {
    
      # http://www.meds-sdmm.dfo-mpo.gc.ca/zmp/climate/AirTemp_f.asp?stn=SHEARWAT
      loc = "http://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/zmp/climate/AirTemp/odf_data_files/SHEARWAT_Monthly_Air_Temp.odf"
      fn = file.path( osd.dir, basename(loc) )
      if ( source=="t.halifax.ns.refresh") download.file( url=loc, destfile=fn )

      test = readLines(fn, n=200)
      nheaderlines = grep( "-- DATA --", test, ignore.case=T)
      data = read.table( fn, skip=nheaderlines, as.is=T)

      names(data) = c("date", "t.halifax.C")
      x = unlist (strsplit( data$date, " ", fixed=T ))
      x = matrix(x, ncol=2, byrow=T)
      data$date = chron( dates.=x[,1], format=c(dates="d-mon-year"), out.format=c(dates="year-m-d")   )
    
      return (data )
    }


    if ( source %in% c("nao", "nao.refresh")) {
       
      # http://www.meds-sdmm.dfo-mpo.gc.ca/zmp/climate/NAO_e.htm
      loc = "http://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/zmp/climate/NAO/odf_data_files/NAOIndex.odf"
      fn = file.path( osd.dir, basename(loc) )
      if ( source=="nao.refresh") download.file( url=loc, destfile=fn )

      test = readLines(fn, n=200)
      nheaderlines = grep( "-- DATA --", test, ignore.case=T)
      data = read.table( fn, skip=nheaderlines, as.is=T)

      names(data) = c("date", "nao")
      x = unlist (strsplit( data$date, " ", fixed=T ))
      x = matrix(x, ncol=2, byrow=T)
      data$date = chron( dates.=x[,1], format=c(dates="d-mon-year") , out.format=c(dates="year-m-d")  )
      data$nao[ which (data$nao < -10) ] = NA

      return (data )
    }


 
    if ( source %in% c("halifax.harbour.sst", "halifax.harbour.sst.refresh")) {
      
      loc = "http://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/zmp/Climate/CoastalSST/odf_data_files/Halifax_SST.odf"
      fn = file.path( osd.dir, basename(loc) )
      
      if ( source=="halifax.harbour.sst.refresh")  download.file( url=loc, destfile=fn )

      test = readLines(fn, n=200)
      nheaderlines = grep( "-- DATA --", test, ignore.case=T)
      data = read.table( fn, skip=nheaderlines, as.is=T)

      names(data) = c("date", "halifax.sst")
      x = unlist (strsplit( data$date, " ", fixed=T ))
      x = matrix(x, ncol=2, byrow=T)
      data$date = chron( dates.=x[,1], format=c(dates="d-mon-year") , out.format=c(dates="year-m-d")  )

      return (data )
    }


    if ( source %in% c("gulf.stream.front", "gulf.stream.front.refresh")) {
      
      loc = "http://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/zmp/climate/GulfSlope/odf_data_files/GSMNTHMN_Monthly_Averages.odf"
      fn = file.path( osd.dir, basename(loc) )
      
      if ( source=="gulf.stream.front.refresh")  download.file( url=loc, destfile=fn )

      test = readLines(fn, n=200)
      nheaderlines = grep( "-- DATA --", test, ignore.case=T)
      data = read.table( fn, skip=nheaderlines, as.is=T)

      names(data) = c("date", "lat", "lon", "stdev", "n" )
      x = unlist (strsplit( data$date, " ", fixed=T ))
      x = matrix(x, ncol=2, byrow=T)
      data$date = chron( dates.=x[,1], format=c(dates="d-mon-year"), out.format=c(dates="year-m-d")   )

      return (data )
    }

    if ( source %in% c("ss.slope.front", "ss.slope.front.refresh")) {
      
      loc = "http://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/zmp/climate/GulfSlope/odf_data_files/SSMNTHMN_Monthly_Averages.odf"
      fn = file.path( osd.dir, basename(loc) )
      if ( source=="ss.slope.front.refresh")  download.file( url=loc, destfile=fn )

      test = readLines(fn, n=200)
      nheaderlines = grep( "-- DATA --", test, ignore.case=T)
      data = read.table( fn, skip=nheaderlines, as.is=T)

      names(data) = c("date", "lat", "lon", "stdev", "n" )
      x = unlist (strsplit( data$date, " ", fixed=T ))
      x = matrix(x, ncol=2, byrow=T)
      data$date = chron( dates.=x[,1], format=c(dates="d-mon-year"), out.format=c(dates="year-m-d")   )

      return (data )
    }




    if ( source %in% c("sable.meteorological", "sable.meteorological.refresh")) {
    
      ## http://www.climate.weatheroffice.ec.gc.ca/prods_servs/documentation_index_e.html
      outfile = file.path( project.datadirectory("indicators"), "data", "osd", "sable.met.merged.rdata")  ## large file .. save a merged instance
      if ( source=="sable.meteorological") {
        load( outfile)
        return(data)
      }

      if ( source=="sable.meteorological.refresh") {
      
       # http://www.meds-sdmm.dfo-mpo.gc.ca/zmp/climate/AirTemp_f.asp?stn=SHEARWAT
        loc.met = "http://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/zmp/met/data/Sable_Island_HLY01.zip"
        loc.rain = "http://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/zmp/met/data/Sable_Island_HLY03.zip"
        loc.rad = "http://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/zmp/met/data/Sable_Island_HLY11.zip"

        fn.met  = file.path( osd.dir, basename(loc.met) )
        fn.rain  = file.path( osd.dir, basename(loc.rain) )
        fn.rad  = file.path( osd.dir, basename(loc.rad) )
       
        download.file( url=loc.met, destfile=fn.met )
        download.file( url=loc.rain, destfile=fn.rain )
        download.file( url=loc.rad, destfile=fn.rad )
      
        fn0 = unz(fn.met, "Sable_Island_HLY01_1.txt")
        test = readLines( fn0, n=200)
        nheaderlines = grep( "end_of_header", test, ignore.case=T, perl=T, useBytes=T)
        d.met = read.table( fn0, skip=nheaderlines, as.is=T)
# [18] "Flags|Indicateurs :"                                                                                   
# [19] "\t 0 : Missing|Manquante "                                                                             
# [20] "\t 1 : Estimated|Estim\xe9e "                                                                          
# [21] "\t 2 : Good|Bonne "            
        names(d.met) = c("year", "month", "day", "hour", "P.sealevel.hPa", "P.sealevel.hPa.flag", "t.dewpoint.C", "t.dewpoint.C.flag", "t.dry",  "t.dry.flag", "wind.speed.horizontal", "wind.speed.horizontal.flag", "wind.direction.from", "wind.direction.from.flag", "total.cloud.prop", "total.cloud.prop.flag" )


        fn0 = unz(fn.rain, "SABLE_ISLAND_HLY03.txt")
        test = readLines( fn0, n=200)
        nheaderlines = grep( "end_of_header", test, ignore.case=T, perl=T, useBytes=T)
        d.rain = read.table( fn0, skip=nheaderlines, as.is=T)
        names(d.rain) = c("year", "month", "day", "hour", "rain.mm", "rain.mm.flag")
# [8] "Flags|Indicateurs :"                                                    
# [9] "\t 0 : Missing|Manquante"                                               
#[10] "\t 1 : Estimated|Valeur estim\xe9e"                                     
#[11] "\t 2 : Freezing|Vergla\xe7antes"                                        
#[12] "\t 3 : Unadjusted|Non ajust\xe9 "                                       
#[13] "\t 4 : Freezing and unadjusted|Vergla\xe7antes et valeur non ajust\xe9e"
#[14] "\t 5 : Trace|Trace"                                                     
#[15] "\t 6 : Valid data|Donn\xe9e valide"     
       

        fn0 = unz(fn.rad, "SABLE_ISLAND_HLY11_1.txt")
        test = readLines( fn0, n=200)
        nheaderlines = grep( "end_of_header", test, ignore.case=T, perl=T, useBytes=T)
        d.rad1 = read.table( fn0, skip=nheaderlines, as.is=T)

# [11] "Flags|Indicateurs :"                                                           
# [12] "\t 0 : Missing|Manquante"                                                      
# [13] "\t 1 : Trace|Traces"                                                           
# [14] "\t 2 : Valid datum, uknown snow|Donn\xe9e valide, neige inconnue"              
# [15] "\t 3 : Valid datum, no snow cover|Donn\xe9e valide, pas de couverture neigeuse"
# [16] "\t 4 : Valid datum, snow cover|Donn\xe9e valide, couverture neigeuse"          
# [17] "\t 5 : Estimated, unknown snow|Estim\xe9e, neige inconnue"                     
# [18] "\t 6 : Estimated, no snow cover|Estim\xe9e, pas de couverture neigeuse"        
# [19] "\t 7 : Estimated, snow cover|Estim\xe9e, couverture neigeuse"          
        
        fn0 = unz(fn.rad, "SABLE_ISLAND_HLY11_2.txt")
        test = readLines( fn0, n=200)
        nheaderlines = grep( "end_of_header", test, ignore.case=T, perl=T, useBytes=T)
        d.rad2 = read.table( fn0, skip=nheaderlines, as.is=T)

        d.rad = rbind(d.rad1, d.rad2 )
        names(d.rad) = c("year", "month", "day", "hour", "rad.global.kJ.m2", "rad.global.kJ.m2.flag", "rad.net.kJ.m2" , "rad.net.kJ.m2.flag" )
        
       #  to.drop = which( d.rad$rad.global.kJ.m2.flag %in% c(0, 1, 5, 6, 7) |  d.rad$rad.net.kJ.m2.flag %in% c(0, 1, 5, 6, 7)) # remove estimated data
       #  d.rad = d.rad[ - to.drop , ]

        data = merge( d.met, d.rain, by=c("year", "month", "day", "hour"), all.x=T, all.y=F, sort=F)
        data = merge( data, d.rad, by=c("year", "month", "day", "hour"), all.x=T, all.y=F, sort=F)

        data$date = chron( dates.=paste(data$day, data$month, data$year, sep="-"), format=c(dates="d-m-y"), out.format=c(dates="year-m-d")   )
      
        save( data, file=outfile, compress=T)

        return (data )
      }  # end refresh

    }


  }

