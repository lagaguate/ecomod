
observer.load.rawdata = function(file) {
  out = NULL
  rawdata=NULL
  header = scan( file, what="", sep="\n", strip.white=F )  # there are control characters at the end of a file on occasion
  header = header[which(nchar(header)>140)]
  tmpfile =  file.path(tmpdir, make.random.string(".tmp.observer.csv"))

  write.table(header, file=tmpfile, append=F, row.names=F, col.names=F, quote=F )

  if (length(header) > 1) {
  }

  print("debug", 3)
  file.remove(tmpfile)

  return( rawdata )
}


  historical.data = function() {


      loc = "http://tethys.bio.dfo.ca/~jae/projects/snowcrab/data/observer/archive/2003/Zone%2024/Port%20Sample/Semaine%2025/24P03053.txt"
      write.table( scan( url( loc),  what="", sep="\n", strip.white=F ), file="tmp.out",  row.names=F, col.names=F, quote=F, sep=";" )
      dataformat = observer.data.formats()  # see auxillary function, below
      rawdata = read.fwf( file="tmp.out", widths=dataformat$w, as.is=T, colClasses="character", header=F, strip.white=F)
      colnames(rawdata) = dataformat$v
      rawdata = rawdata[, dataformat$keep]
      str(rawdata)

  rawdata = concatenate.historical.data( type="seasamples",   outfile="odb.historical.datadump.seasamples.rdata" )
  rawdata =  concatenate.historical.data( type="portsamples", outfile="odb.historical.datadump.portsamples.rdata" )



  }


  concatenate.historical.data = function(type, outfile) {

    rootdir = file.path( "/home", "jae", "projects", "snowcrab", "data", "observer" )

    directories =  file.path( rootdir, "archive", c(2001:2003) )
    directories =  file.path( rootdir, "archive", c(1996, 1999:2000) )
    directories =  file.path( rootdir, "archive", c(1997, 1998) )

    fl = list.files(path=directories, pattern="[*.txt]$", full.names=T, recursive=T)

    if (type=="seasamples") {
      fl = fl[ grep( "sea sample", fl, ignore.case=T ) ]
    } else if (type=="portsamples") {
      fl = fl[ grep( "port sample", fl, ignore.case=T ) ]
    }

    temp.output = "tmp.dump"
    if (file.exists( temp.output ) )  file.remove(temp.output)
    for (i in fl) {
      print ( paste("reading",i) )
      out = NULL
      rawdata=NULL
      read0 = scan( file=i, what="", sep="\n", strip.white=F )  # there are control characters at the end of a file on occasion
      read0 = read0[which(nchar(read0)>140)]
      write.table( read0, file=temp.output, append=T, row.names=F, col.names=F, quote=F, sep=";" )
    }

    # data format is column-width structured
    dataformat = observer.data.formats(2003)
    dataformat = observer.data.formats(1997)

    rawdata = read.fwf( file=temp.output, widths=dataformat$w, as.is=T, colClasses="character", header=F, strip.white=F)
    colnames(rawdata) = dataformat$v
    rawdata = rawdata[, dataformat$keep]

    save( rawdata, file="seasamples.1996.1999.2000.rdata", compress=T)
    save( rawdata, file="portsamples.1996.1999.2000.rdata", compress=T)

    save( rawdata, file="seasamples.1997.1998.rdata", compress=T)
    save( rawdata, file="portsamples.1997.1998.rdata", compress=T)

    save( rawdata, file=outfile, compress=T)

    save( rawdata, file=outfile, compress=T)
    if (file.exists( temp.output ) )  file.remove(temp.output)

    return( "Done" )

  }


