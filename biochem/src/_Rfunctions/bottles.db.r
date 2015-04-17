
bottles.db = function( DS, p, plotdata=FALSE ) {
 # Gordana Lazin, 8 April 2015, first version
  
  # data view of nutrients and chl from biochem db
  
  biochem.dir = project.directory("biochem") 
  biochem.data.dir = file.path( biochem.dir, "data" ) 
  biochem.datadump.dir = file.path( biochem.dir, "data", "datadump" ) 
  
  if ( DS == "bottles.dump.odbc" ) {
    if (FALSE) {
      
      "
      # Shelley's BioChem query for nutrients and chl      
      create or replace view climatology as (
        SELECT m.data_center_code, m.name, m.descriptor mission, m.start_date, m.end_date,
        e.collector_event_ID,e.collector_station_name,
        h.start_date header_start,
        e.start_date event_start,
        h.start_time header_start_time,
        e.start_time event_start_time,
        h.end_date header_end,
        e.end_date event_end,
        h.end_time header_end_time,
        e.end_time event_end_time,
        h.time_qc_code,
        h.start_lat header_start_lat,
        h.start_lon header_start_lon,
        h.end_lat header_end_lat,
        h.end_lon header_end_lon,
        h.position_qc_code,
        h.start_depth header_start_depth,
        h.end_depth header_end_depth,
        h.sounding header_sounding,
        d.collector_sample_id sample_id,
        d.data_type_seq,
        dt.data_retrieval_seq,
        dt.method,
        dt.description data_type,
        dt.priority,
        d.data_value data_value,
        u.name unit_for_data,
        d.data_qc_code,
        d.averaged_data
        FROM biochem.bcdiscretedtails d, biochem.bcdiscretehedrs h,biochem.bcevents e,
        biochem.bcmissions m, biochem.bcdatatypes dt, bcunits u,
        bcdataretrievals dr
        WHERE d.discrete_seq=h.discrete_seq
        AND h.event_seq=e.event_seq 
        and d.data_type_seq=dt.data_type_seq
        AND e.mission_seq=m.mission_seq
        AND dt.unit_seq=u.unit_seq
        and  (upper(dr.parameter_description)= 'CHLOROPHYLL A'
              or upper(dr.parameter_description) like '%SILICATE%'
              or upper(dr.parameter_description) like '%NITRATE%'
              OR (upper(dr.parameter_description) like '%PHOSPH%' and method like 'PO4%'))
        AND dt.data_retrieval_seq = dr.data_retrieval_seq
        and (h.start_lat BETWEEN 37 AND 48 AND h.start_lon BETWEEN -71 AND -48));
      
      # QA/QC done locally
      #and upper(dr.parameter_description) not like '%UPTAKE%'
      #and to_number(to_char (h.start_date, 'YYYY')) BETWEEN to_number(to_char (m.end_date,'YYYY'))
      #and to_number(to_char (m.start_date,'YYYY'))
      #and to_char(e.start_date, 'DD-MON-YYYY') = to_char(h.start_date, 'DD-MON-YYYY');
      #and h.position_qc_code in (0,1,5)
      #and d.data_qc_code in (0,1,5)
      #and d.data_value >0
      
      Shelley: The script which pulls your data is
      
      select * from climatology
      where
      to_number(to_char (header_start, 'YYYY')) BETWEEN to_number(to_char (end_date,'YYYY'))
      and to_number(to_char (start_date,'YYYY'))
      and to_char(event_start, 'DD-MON-YYYY') = to_char(header_start, 'DD-MON-YYYY');
      
      "
    }
    
    
    require(RODBC)
    dir.create( biochem.datadump.dir, recursive=TRUE, showWarnings=FALSE )
    con = odbcConnect( dsn=oracle.biochem.server, uid=oracle.biochem.user, pwd=oracle.biochem.password, believeNRows=F)
        

    for ( YR  in p$bottles.years ) {
      query = paste( 
      "select * from biochem.climatology where to_number(to_char (header_start, 'YYYY')) 
        BETWEEN to_number(to_char (end_date,'YYYY'))
        and to_number(to_char (start_date,'YYYY'))
        and to_char(event_start, 'DD-MON-YYYY') = to_char(header_start, 'DD-MON-YYYY') 
        and to_number(to_char (start_date,'YYYY')) =",  YR, ";" )
      fn = file.path( biochem.datadump.dir, paste( "bottles.dump.odbc", YR, ".rdata", sep="")  )  
      res = NULL
      res = sqlQuery(con, query )
      save(res, file=fn, compress=TRUE )
      print(YR)
    }
    
  }

  
  if ( DS == "bottles.odbc.all.redo" ) {
    fn.all = file.path( biochem.datadump.dir, paste( "bottles.dump.odbc", "all_years", "rdata", sep=".")  )  
    out = NULL
    for ( YR  in p$bottles.years ) {
      fn = file.path( biochem.datadump.dir, paste( "bottles.dump.odbc", YR, ".rdata", sep="")  )  
      res = NULL
      if (file.exists(fn)) load( fn )
      out = rbind( out, res )
    }
    save(out, file=fn.all, compress=TRUE)
  }
  
  if ( DS == "bottles.odbc.all" ) {
    fn.all = file.path( biochem.datadump.dir, paste( "bottles.dump.odbc", "all_years", "rdata", sep=".")  )  
    out= NULL
    if (file.exists(fn.all)) load(fn.all) 
    return( out)
  }
  
  if ( DS %in% c("bottles.qa.qc", "bottles.qa.qc.redo")  ) {
    fn = file.path( biochem.data.dir, paste( "bottles.qa.qc", ".rdata", sep="") )  
    if ( DS == "bottles.qa.qc" ) {
      out = NULL
      if (file.exists(fn)) load(fn)
      return(out)
    }
    
    nc = bottles.db( DS="bottles.odbc.all", p=p ) 
    
   # data filtering is in plot_chl_nut.r
   names(nc)=tolower(names(nc))
   
   # replace factor with characters
   i = sapply(nc, is.factor)
   nc[i] <- lapply(nc[i], as.character)
   
   # === Data filtering starts here ====
  
   
   # find flagged data (2 inconsistent, 3 doubtful, 4 erroneous)
   flagged=which(nc$position_qc_code %in% c(2,3,4) | nc$data_qc_code %in% c(2,3,4))
   
   # find data less than 0
   neg=which(nc$data_value<0)
   
   # find methods that have "Uptake" in method field
   uptake=grep("Uptake",nc$method)
   
   
   # make a list of missions with bad records for each parameter
   bm=list(bmc=c("OC7908","32G879008"),bmp=c("18HU88026"),bms=c("18HU167005","31TR26870","180167005"))
   
   # find indices for bad missions for each parameter (no bad missions for nitrate)
   bc=intersect(grep("Chl", nc$method), which(nc$mission %in% bm$bmc) )
   bp=intersect(grep("PO4", nc$method), which(nc$mission %in% bm$bmp) ) 
   bs=intersect(grep("SiO4", nc$method), which(nc$mission %in% bm$bms) ) 
   
   # find duplicated records. Create uid and uid1 to spot duplicates
   nc$uid=paste(nc$mission,"_",nc$collector_event_id,"_",nc$sample_id,sep="")
   nc$uid1=paste(nc$uid,"_", nc$method, sep="")
   
   dup=which(duplicated(nc$uid1)) # duplicated records
   
   
   # remove identified records and create filtered nutrient-chlorophyll dataset ncf
   ncf=nc[-unique(c( flagged,neg,uptake,bc,bp,bs,dup)) ,] 
   
   rm(nc)
   
   # ==== flag coastal and ocean data ====
   
   # load polygons 5km away from coastline (dataframe is c5kf)
   fn=find.ecomod.gis("coast5km.polygons.4filtering")
   c5kf=read.table(fn)
   
   # add coastFlag field (1 for open ocean, 2 for coastal ocean)
   # set coastFlag to 1 for all records
   ncf$coastFlag=1
   
   no_poly=length(unique(c5kf$flag))
   
   library(sp)
   # find points in polygons. These are within 5km off coast; flag those as 2
   for (i in 1:no_poly) {
     pol=which(c5kf$flag==i)
     a = which( point.in.polygon( ncf$header_start_lon, ncf$header_start_lat,
                                  c5kf$lon[pol], c5kf$lat[pol]) != 0 )
     ncf$coastFlag[a]=2
     
   }
   # === ncf$coastalFlag is 1 for ocean data, and 2 for coastal data === 
   
   
   # add param column with values chl,nit, pho, sil
   # so it will be easier to separate the parameters
   ncf$param=NA
   ncf$param[grep("Chl",ncf$method)]="chl"
   ncf$param[grep("NO3",ncf$method)]="nit"
   ncf$param[grep("PO4",ncf$method)]="pho"
   ncf$param[grep("SiO4",ncf$method)]="sil"
   
   
   # Data filtering based on IML bottle data quality control described on:
   # http://slgo.ca/app-sgdo/en/docs_reference/botl_odf_quality.html
   # Defines acceptable range of values for NW Atlantic for nutrients and chl, valid for open ocean only
   
   # filter ocean data only according to IML range limits for NW Atlantic (IML Test 2.1)
   chlF=which(ncf$param=="chl" & (ncf$data_value<0 | ncf$data_value>50) & ncf$coastFlag==1)
   nitF=which(ncf$param=="nit" & (ncf$data_value<0 | ncf$data_value>515) & ncf$coastFlag==1)
   phoF=which(ncf$param=="pho" & (ncf$data_value<0 | ncf$data_value>4.5) & ncf$coastFlag==1)
   silF=which(ncf$param=="sil" & (ncf$data_value<0 | ncf$data_value>250) & ncf$coastFlag==1)
   
   # IML Test 2.4 -- deep values for silicate and phosphate cannot be below 0.01
   dsil=which(ncf$param=="sil" & (ncf$data_value<0.01 | ncf$data_value>250) 
              & ncf$coastFlag==1 
              & (ncf$header_start_depth>=150 & ncf$header_start_depth<=900))
   
   dpho=which(ncf$param=="pho" & (ncf$data_value<0.01 | ncf$data_value>4.5) 
              & ncf$coastFlag==1 
              & (ncf$header_start_depth>=150 & ncf$header_start_depth<=1500))
   
   # indices of all the records that do not pass IML Test 2.1 and 2.4 (out of range)
   out_of_range=unique(c(chlF,nitF,phoF,silF,dsil,dpho))
   
   # remove out of range records
   ncf=ncf[-out_of_range,]
   
   
   # add month and year
   require(lubridate)
   ncf$month=month(ncf$header_start)
   ncf$year=year(ncf$header_start)
   
   
   # ===== look for overlaps between the methods =====
   # in this dataset there are overlapps only for chlorophyll
   
   # define which parameter will be investigated
   df=ncf[which(ncf$param=="chl"),]
   
   # c contains unique methods
   c=unique(df$method)
   l=length(c)
   
   # check for overlaps two methods at the time
   # have to consider all combination of the methods
   # install.packages("combinat")
   require(combinat)
   # p contains all combinations of two methods
   # i.e. method1 vs method2, method1 vs method3, method2 vs method3 etc.
   p=combn(l,2)
   no_combinations=dim(p)[2]
   
   
   # initiate lists containing number of overlaps and overlapping uid
   no_overlaps=NULL
   overlapping_uid=list()
   
   
   # find overlaping sample id's for each method and store them in the list
   # if there are any overlaps plott scatter plot, otherwise don't do anything
   
   for (i in 1:no_combinations) {
     
     # define which methods are compared
     method1=c[p[1,i]]
     method2=c[p[2,i]]
     
     # overlapping samples
     o=intersect(df$uid[which(df$method==method1)],df$uid[which(df$method==method2)])
     
     # lists that contains overlapping uid and number of overlaps
     overlapping_uid[[i]]=o
     no_overlaps[i]=length(o)
     
     # if there are overlaps plot the scatter plot
     
     if (plotdata) {
       if (length(o)>0) {
         # indices of overlapping samples
         im1=which(df$method==method1 & df$uid %in% o)
         im2=which(df$method==method2 & df$uid %in% o)
         
         # uid are not in the same order. Need to merge based on uid
         dfm=merge(df[im1,],df[im2,],by="uid")
         
         # limit for the axis so x and y axis have the same range
         axlim=c(0,ceiling(max(c(dfm$data_value.x,dfm$data_value.y)))+1)
         
         # plot scatter plot for two methods
         plot(dfm$data_value.x,dfm$data_value.y,xlab=method1, ylab=method2, 
              xlim=axlim, ylim=axlim, col="dodgerblue", 
              main=paste("Number of overlapping samples:",length(o)), cex.main=0.9)
         abline(0,1)
         ll=lm(dfm$data_value.y ~ dfm$data_value.x)
         abline(ll, col="red",lwd=2)
         
         # get r2, intercept and slope from the linear regression
         r2=formatC(summary(ll)$r.squared, format="f", digits=3)
         intc=formatC(coef(ll)["(Intercept)"], format="f", digits=3)
         sl=format(coef(ll)["dfm$data_value.x"], format="f", digits=3)
         eq=paste("y=",sl,"x +", intc,sep="")
         r=paste("R2=", r2, sep="")
         legend("topleft",c(eq,r),bty="n")
         
         #plot overlapping samples on the map
         require(maps)
         require(mapdata)
         coastline = map( "worldHires", regions=c("Canada", "USA"), xlim=c(-71, -48 ), ylim=c(37, 50), fill=FALSE, plot=FALSE )
         plot( coastline$x, coastline$y, pch="." , xlab="Longitude", ylab="Latitude", 
               main=paste(length(o), " overlapping samples\n", method1, "-", method2), cex.main=0.9)
         points(dfm$header_start_lon.x,dfm$header_start_lat.x,cex=0.7, col="dodgerblue")
         
       }
     }
   }
   # ===== end for checking for overlaps ======
   # have to decide what to do with overlapping points, 
   # which method to choose, or to correct
   
   
   
   # it was decided to remove Welschmayer chl for overlapping samples
   # First, make a character vector from all overlapping uid
   overlap_uid=unlist(overlapping_uid)
   
   # find indices for Welschmayer in overlapping samples
   mw=which(ncf$method=="Chl_a_Welschmeyer_sF" & ncf$uid %in% overlap_uid)
   
   # delete overlapping Welschmayer Chl from the dataframe
   ncf=ncf[-mw,]
   
   # ===== Data filtering complete ======
   
   
    
    out=ncf
    
    save(out, file=fn, compress=TRUE )
    return( "Completed filtering step" )
  }
  
  
}

