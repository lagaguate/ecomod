#This accepts a df and generates a table for inclusion in the KML balloon
PortSamples.make.kml.table <- function(df,main="",center="F"){
  #This stuff goes in the balloon - primarily a table that shows the contents of the first record, 
  #with column headings converted to row names
  
  dfNames<-names(df)
  dfTable <- as.data.frame(t(df))
  dfTable <- cbind(dfNames,dfTable)
  theTable<-NULL
  theTable<-paste(theTable, "\n\t\t<table border=1'>",sep="")
  ncols = length(dfTable)
  nrows = length(dfTable[[1]])
  if (length(nrows)>0) {
    for (row in 1:nrows) {
      theTable<-paste(theTable, "\n\t\t\t<tr>",sep="")
      for (col in 1:ncols ) {
        if (col==1){
          flag_vrn<-F
          if(dfTable[row , col]=="VRN"){
            flag_vrn<-T
          }
          
          if (dfTable[row , col]=="LAT" ||dfTable[row , col]=="LON"){
            dfTable[row , col]<-paste(dfTable[row , col], " (Decimal Degrees)",sep="")
          }
          theTable<-paste(theTable, "\n\t\t\t\t<td><strong>",dfTable[row , col],"</strong></td>",sep="")
        }else{
          if(flag_vrn==T){
            dfTable[row , col]<-paste("<A href='http://foip.ent.dfo-mpo.ca/foip/vesselsearch.php?vrn=",dfTable[row , col], "'>",dfTable[row , col],"</a>",sep="")
          }
          theTable<-paste(theTable, "\n\t\t\t\t<td>",dfTable[row , col],"</td>",sep="")
        }
      }
      theTable<-paste(theTable, "\n\t\t\t</tr>",sep="")
    }
  }
  theTable<-paste(theTable, "\n\t</table>",sep="")
}
