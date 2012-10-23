
# -----------------
# load scanmar data (netminding data which unfortunately has the same nae as the "scanmar" database file

  if (from.ascii.data) {
    smar = get.netmind(outfile = "smar.csv")
    save(smar, file="smar", compress=T)
  }
  # load("smar")

  # base routines not finished yet



# --------------------------------------------------
# primary data read routine for scanmar datafiles

  get.scanmar = function(outfile) {
    file.remove(outfile)
    directories =  
      c("~/data/snowcrab/moncton/monctondatadump/Trawl_surveys/1999/Scanmar\ Data",
        "~/data/snowcrab/moncton/monctondatadump/Trawl_surveys/1998/Scanmar\ Data",
        "~/data/snowcrab/moncton/monctondatadump/Trawl_surveys/1997/Scanmar\ Data",
        "~/data/snowcrab/moncton/monctondatadump/Trawl_surveys/1996/Scanmar\ Data"
       )
    for (d in directories) {
      files = list.files(path=d, pattern="*.scd", full.names=T, recursive=T)
      for (i in files) {
        print (i)
        j = load.scanmar (file = i)
        if (!is.null(j)) write.table(j, file=outfile, append=T, row.names=F, col.names=F, quote=F, sep=" ")
      }
    }
    scanmar= read.table(outfile, as.is=T, colClasses="character", header=F, sep="")


######     scanmar = scanmar[,c(3:8, 10:12, 14:15, ..... )  ####### not finished ...
#######
#######
      
    labels = c( "smlat", "smlon", "nmspeed", "nmprimary", "nmsecondary", "nmdoorspread", 
                "nmdepth", "nmfilename")
    colnames(scanmar) = labels
    numerics = c("nmlat", "nmlatmin", "nmlon", "nmlonmin", "nmspeed", 
                 "nmprimary", "nmsecondary", "nmdoorspread", "nmdepth")
    for(i in numerics) scanmar[,i] = as.numeric(scanmar[,i])
    require(date)
    year = as.numeric(substring(scanmar$date, 1, 2))
    year = ifelse(year <0, 1900+year, 2000+year)
    month = as.numeric(substring(scanmar$date, 3, 4))
    day = as.numeric(substring(scanmar$date, 5, 6))
    scanmar$Date = mdy.date(month=month, day=day, year=year)
    return(scanmar)
  }


# ------------------------------------
# low-level scanmar data read routine
# concatenate raw data files

  load.scanmar = function(file) {
    scanmar=NULL
    header = readLines(file, n=10)
    if (length(header) > 9) {
      direct = readLines(file)
      scanmar = data.frame(direct=direct[8:(length(direct)-2)])
  
      scanmar$direct = gsub("[\"]", " ", as.character(scanmar$direct))
      scanmar$scanmarfilename = gsub("[\t\" ]", "", unlist(strsplit(header[1], ":"))[2])
      scanmar$startdaytime = gsub("[\t\"]", "", unlist(strsplit(header[2], ": "))[2])
      scanmar$scanmarcomments = gsub("[,.;\t\" ]", "", unlist(strsplit(header[4], ": "))[2])
    }
    return(scanmar)
}



