#This accepts a df and generates a table for inclusion in the KML balloon
oceans.make.kml.table <- function(df,main="",center="F"){
  #This stuff goes in the balloon - primarily a table that shows the contents of the first record, 
  #with column headings converted to row names
  dfNames<-names(df)
  dfTable <- as.data.frame(t(df))
  dfTable <- cbind(dfNames,dfTable)
  theTable<-("\n\t\t<table border=1'>")
  ncols = length(dfTable)
  nrows = length(dfTable[[1]])
  thisSource = df$SOURCE[1]
  if (length(nrows)>0) {
    for (row in 1:nrows) {
      theTable<-paste(theTable, "\n\t\t\t<tr>",sep="")
      for (col in 1:ncols ) {
        if (col==1){
          flag_lic<-F
          flag_vrn<-F
          if(dfTable[row , col]=="LICENCE_ID"){
            flag_lic<-T
          }
          if(dfTable[row , col]=="VR_NUMBER"){
            flag_vrn<-T
          }
          #warn if data is "guessed" from logbook
          if (thisSource=="VMS" && (dfTable[row , col]=="VESSEL_NAME" || dfTable[row , col]=="LICENCE_ID" || dfTable[row , col]=="GEAR" ||dfTable[row , col]=="HAIL_OUT_SPECIES" ||dfTable[row , col]=="HAIL_OUT_SPECIES_CATEGORY" ||dfTable[row , col]=="SPECIES_CATEGORY")){
            dfTable[row , col]<-paste(dfTable[row , col], "*",sep="")
          }
          
          if (dfTable[row , col]=="LAT" ||dfTable[row , col]=="LON"){
            dfTable[row , col]<-paste(dfTable[row , col], " (Decimal Degrees)",sep="")
          }
          theTable<-paste(theTable, "\n\t\t\t\t<td><strong>",dfTable[row , col],"</strong></td>",sep="")
        }else{
          if(flag_lic==T){
            dfTable[row , col]<-paste("<A href='http://foip.ent.dfo-mpo.ca/foip/licence.php?licence=",dfTable[row , col], "'>",dfTable[row , col],"</a>",sep="")
          }
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
  return(theTable)
}
