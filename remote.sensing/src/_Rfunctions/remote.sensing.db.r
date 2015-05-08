remote.sensing.db = function( DS, polygon="", param="chl" ) {
  # Gordana Lazin 24 April 2014

  # source( file.path("C:","Users", "choij", "Documents", ".Rprofile"))

  remote.sensing.dir = project.datadirectory("remote.sensing")
  
  if (DS=="download") {
    #Download remote sensing data: 
    #1. makes a list of bi-weekly files available on the server (works for chl, sst and tsm)
    #2. Download the files from the list (not working)
    
    # install.packages(RCurl)
    library(RCurl)
    
    # sea surface temperature data, sst
    # meris total suspended matter data, tsm
    # primary production, monthly - has different file naming convention, pp
    # modis chlorophyll-a data, chl
    
    url= switch( DS,
      chl = "ftp://ftp1.dfo-mpo.gc.ca/bodata/bo/hmm/modis/chl/geotiff/", 
      pp="ftp://ftp1.dfo-mpo.gc.ca/bodata/bo/PP/2002-2014_monthly_geotiffs/",
      sst="ftp://ftp1.dfo-mpo.gc.ca/bodata/bo/hmm/noaa/geotiff/",
      tsm="ftp://ftp1.dfo-mpo.gc.ca/bodata/bo/MERIS_RR_TSM/"
    )
    
    # get list of the files in the directory
    filenames= getURL(url, ftp.use.epsv = FALSE,dirlistonly = TRUE) 
    
    # split the string to get the list
    fl = unlist(strsplit(filenames, "\r*\n"))
    
    # files with climatology
    clim=grep("clim",fl)
    
    # files with anomaly
    anom=grep("anomal",fl)
    
    # remove anomaly and climatology files from the list
    if (length(c(clim,anom))>0) {fl=fl[-unique(c(clim,anom))]}
    
    
    # make list containing only only weekly filenames
    if ( DS != "pp" ) {
      bwl = fl[c(grep("a1",fl),grep("a2",fl),grep("b1",fl),grep("b2",fl))]
    }
    
    # destination directory
    dd=file.path(remote.sensing.dir, DS) 
    
    # list of files with the directories
    dest=paste0(dd, bwl)
    orig=paste0(url ,bwl)
    
    # download file -- not working
    nf = length( bwl)
    for (i in 1:nf) {
      download.file(orig[i], dest[i], method="auto" )
     # h = getURLContent( orig[i], binary=TRUE )
    }
  }
    
  if (DS=="extract.polygon") {
    
    # extract box from tif images
    # param is a parameter to compute (either chl, sst, tsm, or pp)
    # e.g., param="tsm"
    
    require(raster)
    require(rasterVis)
    require(lattice)
    
    
    # path to the remote sensing data
    rd = file.path( remote.sensing.dir, param )

    pp1 = remote.sensing.db( DS="polygon.st.anne" )
    
    # remote sensing part
    # get the list of the remote sensing files
    fl=list.files(rd,pattern=".tif")
    
    # dataframe to write the results
    out=data.frame(matrix(ncol = 12, nrow = length(fl)))
    names(out)=c("filename","mean","stdev","min","max","median",
                 "validPix","totalPix","day","month","year","week")
    
    # load files from the list one by one, extract pixels from polygon, 
    # compute stats and write it to the 'out' data frame 
    
    for (i in 1:length(fl)) {
      
      fni=paste0(rd,fl[i]) # filename to load
      r=raster(fni) # load raster
      
      # filter for invalid pixels
      if (param=="chl") {
        # remove land and clouds (have value -1), 
        # and chl > 28 mg/m3 (Glen Harrison'S limit for max open ocean chl)
        r[(r<0.1|(r>28))]=NA
      }
      
      if (param=="sst") {
        # remove land and clouds (have value -999), 
        # and sst < -5 
        r[r< -5]=NA
      }
      
      if (param %in% c("pp","tsm")) {
        # remove land and clouds (have value -9 for pp, -1 for tsm), 
        # and primary production or tsm <0
        r[r< 0]=NA
      }
      
      #extract data from the polygon
      pd=extract(r,pp1)
      pd=unlist(pd)
      
      # write out the file name, number of valid pixels, and total pixels
      out$filename[i]=fl[i]
      out$validPix[i]=sum(!is.na(pd))
      out$totalPix[i]=length(pd)
      
      # if there are any valid pixels compute stats; otherwise stats will remain NA
      if (out$validPix[i]>0) {
        out$mean[i]=mean(pd,na.rm=TRUE)
        out$stdev[i]=sd(pd,na.rm=TRUE)
        out$min[i]=min(pd,na.rm=TRUE)
        out$max[i]=max(pd,na.rm=TRUE)
        out$median[i]=median(pd,na.rm=TRUE)
      }
      
      
      # extract year and month from file name
      # file naming conventions for each parameter are:
      # chl: AYYYYmmmxx- 
      # sst: YYYYmmmxx-
      # primary production: YYYYMM_
      # tsm: MERYYYYmmmxx-
      # where YYYY is year, mmm is month abbrviation, and xx is 8-day period notation:
      # a1 - day 1-8
      # a2 - day 9-15
      # b1 - day 16-23
      # b2 - day 24- end of the month
      
      if (param =="chl") {
        out$year[i]=as.numeric(substr(fl[i],2,5))
        out$month[i]=which(tolower(month.abb) %in% substr(fl[i],6,8))
        
      } else if (param=="sst") {
        out$year[i]=as.numeric(substr(fl[i],1,4))
        out$month[i]=which(tolower(month.abb) %in% substr(fl[i],5,7))
        
      } else if (param=='pp') {
        out$year[i]=as.numeric(substr(fl[i],1,4))
        out$month[i]=as.numeric(substr(fl[i],5,6))
        out$week[i]=NA
        out$day[i]=NA
        
      } else if (param=="tsm") {
        out$year[i]=as.numeric(substr(fl[i],4,7))
        out$month[i]=which(tolower(month.abb) %in% substr(fl[i],8,10))
        
      }
      
      
      # extract week from the filename and assign a "day" value for each week
      if (length(grep("a1",fl[i]))>0) {
        out$week[i]=1
        out$day[i]=5
      } else if (length(grep("a2",fl[i]))>0) {
        out$week[i]=2
        out$day[i]=13
      } else if (length(grep("b1",fl[i]))>0) {
        out$week[i]=3
        out$day[i]=21
      } else if (length(grep("b2",fl[i]))>0){
        out$week[i]=4
        out$day[i]=29  
      } 
      
      
      
    }
    
    
    # where to save the data? it is currently in  "C:/home/choij/work"
    outdir = file.path( remote.sensing.dir, "timeseries" )
    if (!file.exists(outdir)) dir.create( outdir )
    outName=paste0(param,".stAnns.polygon.rdata")
    save(out, file=outName, compress=TRUE )
    
    
    if (plot.data) {
  
    # plot the results
      
      if (param=="chl") {
        lab="Chlorophyll [mg/m3]"
        tit="St.Anns MPA Polygon, MODIS 2002-2015"
      }
      
      if (param=="sst") {
        lab="SST [C]"
        tit="St.Anns MPA Polygon, SST 1997-2015"
      }
      
      if (param=="pp") {
        lab="Primary production [mg C/m2/day]"
        tit="St.Anns MPA Polygon, Primary Production 2002-2014"
      }
      
      if (param=="tsm") {
        lab="Total Suspended Matter [g/m3]"
        tit="St.Anns MPA Polygon, MERIS 2002-2012"
      }
      
      if (param %in% c("chl","sst","tsm")) {
        
        # 1. timeseries
        #out$date=as.Date(out$year, out$month, out$day)
        #plot(out$date,out$mean,xlab="Time",ylab=lab, 
        #     main=tit, col="dodgerblue")
        
        # 2. climatology
        
        # by 8-day period: define "part of the month"
        out$monthPart=out$month+((2*out$week-1)/8)
        
        clw=tapply(out$mean,out$monthPart,mean,na.rm=TRUE)
        plot(as.numeric(names(clw)),clw,type="o", xlab="Month", 
             ylab=lab, col="darkblue",lwd=1.5,
             cex=1.1,pch=21, bg="dodgerblue", 
             main=tit, xaxt="n")
        axis(1, at=1:12,labels=month.abb)
        
        # sort out by month and year
        out=out[with(out, order(year, monthPart)), ]
        
        # plot by year
        xyplot(mean ~ monthPart| factor(year),
               data=out1,as.table=TRUE,type="b",
               xlab="Month",ylab=lab,
               main=tit)
        
      }
      
      
      # monthly
      clm=tapply(out$mean,out$month,mean,na.rm=TRUE)
      plot(as.numeric(names(clm)),clm,type="b",xaxt="n", xlab="Month", 
           ylab=lab, main=tit, col="dodgerblue",lwd=2, cex=1.2, cex.main=0.9)
      axis(1, at=1:12,labels=month.abb)
      
      # monthly, group by year: lattice plot
      xyplot(mean ~ month| factor(year),
             data=out,as.table=TRUE,
             xlab="Month",ylab=lab,
             main=tit, type='o')
      
    }
    
    
  }
  
  if (DS == "polygon.st.anne" ) {
    
    # path and file name to the St. Anns polygon
    polygon.fn = file.path( remote.sensing.dir, "StAnnsMPA_polygon.csv" )
    
    # load St.Anns polygon
    pol=read.csv( polygon.fn)
    
    # convert to spatial polygon
    pp=Polygon(pol)
    pp1=SpatialPolygons(list(Polygons(list(pp),1)))
    
    return(pp1)
    
  }
  
  
  
}