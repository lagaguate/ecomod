

#########################################
############# Retired functions #########
#########################################



  # -------------------

  clean.minilog.filenames = function(x) {
        fn = gsub("[-_ ]", "",  x)
        fn = gsub("[[:alpha:]]", "", fn)
        fn = gsub("/.$", "", fn)
        fn = gsub("^/.", "", fn)
      return (fn)
  }



  get.minilog.old = function(DS, method, threshold, additional.yrs=NULL) {
    # a threshold of 1 to 1.5 sd provides good convergent solutions with 1 being optimal
    # lower numbers give better fits for regular data, higher numbers better for very irregular bathimetry

    bottom=NULL
    outfile=file.path(R.sc, "bottom.rdata")
    tmpfile =  file.path( tempdir(), make.random.string(".tmp.minilog.csv"))

    if (DS=="file") load(outfile)

    if (DS=="incremental.add") {
      basedir = sc.minilog
      addfiles = as.character(additional.yrs)
      for (d in  file.path(basedir, addfiles) ) {
        files = list.files(path=d, full.names=T, recursive=T)
        badtows = grep("bad", files, ignore.case=T)
        if (length(badtows) >0) files = files[ -badtows ]  # remove tows with "bad" in the filename
        if (length(files) > 0) {
          for (i in files) {
            print (i)
            j = load.minilog (i, method, threshold, dataset="bio")
            if (!is.null(j)) write.table(j, file=tmpfile, append=T, row.names=F, col.names=F, quote=F, sep=";")
          }
        }
      }
      if (file.exists( tmpfile)) {

        bottom.temp = read.table(tmpfile, sep=";", as.is=T, colClasses="character", header=F)
        colnames(bottom) = c( "filename", "yr", "surveytype", "stationid", "SN",
                              "studyid", "z", "t", "zsd", "tsd", "n", "t0", "t1" )
        bottom = factor2number(bottom, c("z", "t", "zsd", "tsd", "n", "yr") )
        bottom$t0 = string2chron(bottom$t0)
        bottom$t1 = string2chron(bottom$t1)
        bottom$dt = bottom$t1 - bottom$t0
        bottom$yr = NULL

        dt.b = as.numeric(bottom$dt)*24*60
        ques = which( dt.b > 15 )
        if (length(ques) > 0 ) {
          print ("strange data:" )
          print (bottom[ques,])
          stop
        }
        new = bottom
        load(outfile)
        bottom = rbind(bottom, new)
        save(bottom, file=outfile, compress=T)
      }

    }

    if (DS=="redo") {
      file.remove(outfile)

      basedir = sc.minilog
      allfiles = list.files(path=basedir, full.names=F)
      gulffiles = c("1998", "1999", "2000", "2001", "2002", "2003") # these are historical data (from Moncton)
      biofiles = setdiff(allfiles, gulffiles )

      # historical data first from Gulf region
      for (d in file.path(basedir, gulffiles) ) {
        files = list.files(path=d, full.names=T, recursive=T)
        badtows = grep("bad", files, ignore.case=T)
        if (length(badtows) >0) files = files[ -badtows ]  # remove tows with "bad" in the filename
        if (length(files) > 0) {
          for (i in files) {
            print (i)
            j = load.minilog(i, method, threshold, dataset="moncton")  # variable naming conventions in the past
            if (!is.null(j)) write.table(j, file=tmpfile, append=T, row.names=F, col.names=F, quote=F, sep=";")
          }
        }
      }

      # 2004+ data from BIO
      for (d in  file.path(basedir, biofiles) ) {
        files = list.files(path=d, full.names=T, recursive=T)
        badtows = grep("bad", files, ignore.case=T )
        if (length(badtows) >0) files = files[ -badtows ]  # remove tows with "bad" in the filename
        if (length(files) > 0) {
          for (i in files) {
            print (i)
            j = load.minilog (i, method, threshold, dataset="bio")
            if (!is.null(j)) write.table(j, file=tmpfile, append=T, row.names=F, col.names=F, quote=F, sep=";")
          }
        }
      }

      if (file.exists( tmpfile)) {
        bottom = read.table(tmpfile, sep=";", as.is=T, colClasses="character", header=F)
        colnames(bottom) = c( "filename", "yr", "surveytype", "stationid", "SN",
                              "studyid", "z", "t", "zsd", "tsd", "n", "t0", "t1" )
        bottom = factor2number(bottom, c("z", "t", "zsd", "tsd", "n", "yr") )
        bottom$t0 = string2chron(bottom$t0)
        bottom$t1 = string2chron(bottom$t1)
        bottom$dt = bottom$t1 - bottom$t0
        bottom$yr = NULL

        dt.b = as.numeric(bottom$dt)*24*60
        ques = which( dt.b > 15 )
        if (length(ques) > 0 ) {
          print ("strange data:")
          print (bottom[ques,])
          stop
        }
        save(bottom, file=outfile, compress=T)
      }
    }
    file.remove(tmpfile)

    return (bottom)
  }


  # ---------------------------------------- low-level data access
  load.minilog = function(file, method, threshold, dataset) {

    out = NULL
    minilog=NULL
    header =  readLines(file, n=8)

    if (length(header) > 7) {  # make sure the file has some data ...
      SN = unlist(strsplit(header[2], "="))[2]
      studyid = gsub("[#]", "_",  gsub("[,; ]", "", unlist(strsplit(header[3], "="))[2]) )
      minilog = as.data.frame(read.table( file=file, sep=",", as.is=T, colClasses="character", header=F, skip=7))
      if (dim(minilog)[1] > 30) {
        fileinfo = tolower(unlist(strsplit(file, split="/")))
        stationid = tolower(unlist(strsplit(basename(file), "\\."))[1])
        surveytype = fileinfo[7]
        yr = fileinfo[9]
        filename = fileinfo[10]

      if (dim(minilog)[2] == 4)  { # depth  not recorded (e.g. 1998)
        labels = c( "mdate", "mtime", "temperature", "depth")
        colnames(minilog) = labels
        numerics = c("temperature", "depth")
        minilog = factor2number(minilog, numerics)

        # date error check:
        minilog$mdate = gsub("^80-", "2000-", minilog$mdate )  # there are incorrect year entries
        test = as.numeric(unlist(strsplit(minilog$mdate[1], "-")))
        error=NA

        if (test[1] > 32 |  test[1]==0) {
          date.format = c( dates="y-m-d", times="h:m:s")
        } else if (test[3] > 32 |  test[3]==0) {
          date.format = c( dates="d-m-y", times="h:m:s")
        } else { # ambiguous
          date.format = c( dates="d-m-y", times="h:m:s")
        }

        minilog$jul = chron( dates.=minilog$mdate, times.=minilog$mtime,
            format=date.format, out.format=c(dates="year-m-d", times="h:m:s") )

        if (is.na(minilog$jul[1])) error = "Ambiguous time format"
        if (years(minilog$jul[1]) != yr)  error = "Years inconsistent"
        strangedatacheck = lm(temperature ~ depth, data=minilog,  na.action="na.omit")
        if (summary(strangedatacheck)$r.squared > 0.95) error="suspicious data"

        l = bottom.temp(minilog, method=method, threshold=threshold)

        if (!is.null(l)) {
          toextract = c( "filename", "yr", "mdate", "surveytype", "stationid", "SN", "studyid")
          outvars  = c( "z", "t", "zsd", "tsd", "n", "t0", "t1" )
          numerics = c( "z", "t", "zsd", "tsd", "n", "yr" )
          out = data.frame(filename, yr, surveytype, stationid, SN, studyid)
          out = cbind(out, l)
          # colnames(minilog) = c( toextract, outvars )
          out = factor2number (out, numerics)

          # error corrections:
          if (out$yr==2006 & out$stationid=="ep124" ) {
            d0 = chron( dates.="2006-10-11", times.="16:37:16", format=c(dates="y-m-d", times="h:m:s"))
            d1 = chron( dates.="2006-10-16", times.="05:40:50", format=c(dates="y-m-d", times="h:m:s"))
            offset = times(d1) - times(d0)
            out$t0 = chron(times(out$t0) + offset, out.format=c(dates="year-m-d", times="h:m:s"))
          }

        }

      }
      }
      }
    return(out)
    }

