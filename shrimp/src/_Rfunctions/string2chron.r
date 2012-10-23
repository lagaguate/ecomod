
string2chron = function(x) {  # used in minilog date functions
  require (chron)
  x = gsub("[()]", "", x)
  y = matrix(unlist(strsplit(gsub("[()]", "", x), " ")),ncol=2,byrow=T)
  out = chron( dates.=y[,1], times.=y[,2], format=c(dates="year-m-d", times="h:m:s"), out.format=c(dates="year-m-d", times="h:m:s"))
  return(out)
}


