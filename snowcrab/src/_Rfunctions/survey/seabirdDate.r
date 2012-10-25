
  
    seabirdDate = function( fnSeaBird=NULL, header=NULL, outvalue="year" ) {
      # input can be file name or the file header
      out = NULL
      if (!is.null( fnSeaBird) && file.exists(fnSeaBird) ) {
        header = readLines( fnSeaBird, n=52)
        if ( any( grepl("Sea-Bird", header) )) {
          lineno = grep( "^start\\ time\\ =", header, perl=T )
          if ( length(lineno)==1 ) {
            date.format = c(dates = "dd-mon-yyyy", times = "h:m:s")
            out.format = c(dates = "yyyy-m-d", times = "h:m:s")
            Mdate = gsub( "^start\\ time\\ =", "", header[lineno])
            Mdate = gsub( "^[[:space:]]{1,}", "", Mdate) # empty space at beginning
            Mdate = gsub( "[[:space:]]{1,}", " ", Mdate) # multiple spaces into one space
            y =  matrix(unlist(strsplit(Mdate, " ")), ncol=4, byrow=T)
            dstring = paste(y[1], y[2], y[3], sep="-") 
            tstring = y[4]
            out = chron( dates.=dstring, times.=tstring, format=date.format, out.format=out.format )   
            if (outvalue=="year") out = as.numeric( as.character( years( out ) ) ) 
            if (outvalue=="date") out = out
            if (outvalue=="format") out = date.format
      }}}
      return (out)
    }


