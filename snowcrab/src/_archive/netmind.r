  # --------------------
  # -------------------RETIRED functions -
  # --------------------


  merge.netmind.old = function(set, netmind, savefile="") {

    ndata0 = dim(set)[1]
    set$netmind.uniqueid = paste( chron2datestamp(set$chrono, res=24), set$station, sep=".")
    set  = merge(set, netmind, by="netmind.uniqueid", all.x=T, all.y=F, sort=F)

    no.sa = which(!is.finite(set$sa))
    set$sa[no.sa] = set$surfacearea[no.sa]  # merge in the BIO data

    ndata1 = dim(set)[1]

    if (ndata1 != ndata0 ) {
      print ("duplicated records ...")
      stop()
    }

    if (savefile != "") save (set, file=savefile, compress=T)

    return(set)
  }


  get.netmind.old = function(DS, set, additional.yrs=NULL) {

    if (DS=="incremental.add") {

        outfile=file.path(sc.netmind, "netmind.csv" )

        file.remove(outfile)
        basedir = sc.netmind
        newfiles = additional.yrs

        indir = file.path(basedir, newfiles)
        for (d in indir) {
          files = list.files(path=d, full.names=T, recursive=T)
          badtows = grep("bad", tolower(files))
          if (length(badtows) >0) files = files[ -badtows ]  # remove tows with "bad" in the filename
          if (length(files) > 0) {
            for (i in files) {
              print (i)
              j = load.netmind (file = i, set[ which(set$yr==as.numeric(basename(d))),] )
              if (!is.null(j)) write.table(j, file=outfile, append=T, row.names=F, col.names=F, quote=F, sep=";")
            }
          }
        }


      new = read.table(outfile, as.is=T, colClasses="character", header=F, sep=";")
      file.remove(outfile)

      labels = c("filename", "netmind.uniqueid", "slon", "slat", "distance", "spread", "spread.sd", "surfacearea", "vel", "vel.sd")
      colnames(new) = labels
      numerics = c("slon", "slat", "distance", "spread", "spread.sd", "surfacearea", "vel", "vel.sd")
      new = factor2number(new, numerics)

      new = new[is.finite(new$surfacearea),]
      new$filename = NULL

      load (file=file.path(sc.netmind,"netmind.rdata"))
      netmind = rbind(new, netmind)
      save (netmind, file=file.path(sc.netmind,"netmind.rdata"), compress=T)

    }

    if (DS=="redo") {
        outfile="netmind.csv"

# ignore historical data .. assume they had it right
# to do: compare a few years with current methods

        file.remove(outfile)
        basedir = sc.netmind
        allfiles = list.files(path=basedir, full.names=F)
        gulffiles = c("1999", "2000", "2001", "2002", "2003") # these are historical data (from Moncton)
        biofiles = setdiff(allfiles, gulffiles )

        indir = file.path(basedir, biofiles)
        for (d in indir) {
          files = list.files(path=d, full.names=T, recursive=T)
          badtows = grep("bad", tolower(files))
          if (length(badtows) >0) files = files[ -badtows ]  # remove tows with "bad" in the filename
          if (length(files) > 0) {
            for (i in files) {
              print (i)
              j = load.netmind (file = i, set)
              if (!is.null(j)) write.table(j, file=outfile, append=T, row.names=F, col.names=F, quote=F, sep=";")
            }
          }
        }


      netmind= read.table(outfile, as.is=T, colClasses="character", header=F, sep=";")
      file.remove(outfile)

      labels = c("filename", "netmind.uniqueid", "slon", "slat", "distance", "spread", "spread.sd", "surfacearea", "vel", "vel.sd")
      colnames(netmind) = labels
      numerics = c("slon", "slat", "distance", "spread", "spread.sd", "surfacearea", "vel", "vel.sd")
      netmind = factor2number(netmind, numerics)

      netmind = netmind[is.finite(netmind$surfacearea),]
      netmind$filename = NULL

      save (netmind, file=file.path(sc.netmind, "netmind.rdata"), compress=T)

    }

    if (DS == "file") load(file.path(sc.netmind,"netmind.rdata"))

    return (netmind)
  }


# -----------------------------------------------
# load netmind ascii datafiles and concatenate

  load.netmind.old = function(file, set) {
    out=NULL
    netmind=NULL
    test = readLines(file, n=7)
    if (length(test) > 6 ) {
      header = test[1:5]
      cmd("gawk '{gsub(\"*\", \"\"); print}'", file, "> tmp.netmind")  # remove "*"
      # skip 16 because first few records are sometimes incomplete
      netmind = read.table( file="tmp.netmind", sep="", as.is=T, colClasses="character", header=F, skip=16)
      colnames(netmind) = c("date", "time", "lat.deg", "lat.min", "lat.orient",
                            "lon.deg", "lon.min", "lon.orient", "speed", "primary", "secondary", "doorspread", "depth")
      numbers = c("lat.deg", "lat.min", "lon.deg", "lon.min", "speed", "primary", "secondary", "doorspread", "depth")
      netmind = factor2number(netmind, numbers)
      netmind$lon = - (netmind$lon.deg + (netmind$lon.min / 60) )
      netmind$lat =    netmind$lat.deg + (netmind$lat.min / 60)
      netmind = netmind[, c("date", "time", "lat", "lon", "speed", "primary", "secondary", "doorspread", "depth")]
      netmind$date = paste(substring(netmind$date,1,2), substring(netmind$date,3,4), substring(netmind$date,5,6), sep="-")
      netmind$time = paste(substring(netmind$time,1,2), substring(netmind$time,3,4), substring(netmind$time,5,6), sep=":")

      localtime = unlist(strsplit(header[2], " ",fixed=T))
      localtime.chron = chron( dates.=paste(localtime[7],localtime[4],localtime[5],sep="-"), times.=localtime[6],
                               format=c(dates="y-mmm-d", times="h:m:s"), out.format=c(dates="year-m-d", times="h:m:s") )
      netmind$chrono = chron( dates.=netmind$date, times.=netmind$time, format=c(dates="y-m-d", times="h:m:s"), out.format=c(dates="year-m-d", times="h:m:s") )
      netmind$chrono = netmind$chrono + as.numeric( localtime.chron - netmind$chrono[1] )  # correct for offset in parentheses

      filename = basename(file)
      trip =  unlist(strsplit(header[4], " ",fixed=T))[4]
      setno =  as.numeric(unlist(strsplit(header[4], " ",fixed=T))[6])
      station = unlist(strsplit(filename, ".", fixed=T))[1]
      station = unlist(strsplit(station, "r", fixed=T))[1]
      station = as.numeric(gsub("[[:alpha:]]", "", station))
      netmind.timestamp = netmind$chrono[floor(dim(netmind)[1]/2)]  # take date stamp from near to the middle of the time series which is generally closer to the real start time


      # ---------------------
      # until the database links between minilog and set are made explicit, we match by time-stamp as in  minilog merge step:  merge.minilog  (in functions.minilog.r)

      resolutions = c(1, 2, 4, 6, 8, 12, 24, 32, 48)
      setinfo = NULL
      for (time.res in resolutions) {
        uniqueid = paste( chron2datestamp(netmind.timestamp, res=time.res), station, sep=".")
        set$netmind.uniqueid = paste( chron2datestamp(set$chrono, res=time.res), set$station, sep=".")
        si = which(set$netmind.uniqueid==uniqueid)
        if (length (si) == 1) {
          setinfo = set[si,]
          break() # exact match
        }
      }

      if (is.null(setinfo)) {
        print("--------------")
        print("Exact match not found:")
        print(filename)
        print(test)
        print("--------------")
      }

      # filter data before sub-selecting ... this allows data smoothing at the terminal ends
      # the Nephrops trawl mouth is 20 m. This is the hard upper limit.
      # When fishing this value is generally observed between 8 to 14 m
      # before processing any values < 7 m or > 15 m are filtered out
      good = # duplicated records ... must check these !NULL
      good = which( netmind$doorspread >= 7 &
                    netmind$doorspread <= 15 &
                    netmind$chrono >= setinfo$t0 &
                    netmind$chrono <= setinfo$t1 &
                    is.finite(netmind$lon) &
                    is.finite(netmind$lat)
                  )
      thresh = 5
      ndata = length(good)
      if (ndata> thresh) {
        n = netmind[good,]
        start = n[1, c("lon", "lat")]
        end =   n[ndata, c("lon", "lat")]
        distance = geodist(point=start, locations=end, method="vincenty") #  in km^2
        spread = mean(n$doorspread, na.rm=T, trim=0.1)/1000  # in km
        spread.sd = sd(n$doorspread, na.rm=T)/1000
        surfacearea = distance* spread # km^2
        vel = mean(n$speed, trim=0.1)
        vel.sd = sd(n$speed)
        out = data.frame( filename, uniqueid, slon=n$lon[1], slat=n$lat[1],
                          distance, spread, spread.sd, surfacearea, vel, vel.sd)
        }

      if (ndata <= thresh) {
          # data looks good but just not enough data points
          out = data.frame( filename, uniqueid, slon=NA, slat=NA,
                            distance=NA, spread=NA, spread.sd=NA, surfacearea=NA, vel=NA, vel.sd=NA)
      }

    }
    return(out)
  }


