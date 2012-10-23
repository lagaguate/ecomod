
  load.minilog.rawdata = function(filename) {
    require(chron)

    out = NULL
    minilog=NULL
    header =  readLines(filename, n=12)
    headerall = paste( header[1:7], collapse="~")

    if (length(header) < 11)  stop()
   
    SN = unlist(strsplit(header[2], "="))[2]
    studyid = gsub("[#]", "_",  gsub("[,; ]", "", unlist(strsplit(header[3], "="))[2]) )
    minilog = as.data.frame(read.table( file=filename, sep=",", as.is=T, colClasses="character", header=F, skip=8))
    
    if ( nrow(minilog) < 10 ) stop()
    
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

        metadata = data.frame(SN, error, fname, headerall)
        out = list( metadata=metadata, minilog=minilog ) 

    return( out )
  }


