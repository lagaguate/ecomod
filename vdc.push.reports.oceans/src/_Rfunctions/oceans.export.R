#MMM Modified Export.R Feb 6, 2014 for use by PushReports

#function to load existing dataframe and extract it into RData and excel files
generateExcel <- function(df_in, xlsName,workdir,tmpdir,savelocation){
  #1   the data
  dfMMM2<<-df_in
  df1        <-as.data.frame(df_in[[1]]) 
 
  #2 Metadata SQL & Bound Variables:
  if (is.null(df_in[[4]])){
    days<-'null'
  }else{
    days<-df_in[[4]]
  }
  if(is.null(df_in[[5]])){
    startDate<-'null'
  }else{
    startDate<-df_in[[5]]
  }
  if(is.null(df_in[[6]])){
    endDate<-'null'
  }else{
    endDate<-df_in[[6]]
  }
  if(is.null(df_in[[7]])){
    vrn<-'null'
  }else{
    vrn<-df_in[[7]]
  }
  
  querySent   <-            df_in[[3]]
  
  
  #3 Other stuff
  filename  <-              df_in[[8]]
  #dwindow   <-              df_in[[9]]
  
  #999 Not wanted in excel
  #vertexFields<-            df_in[[2]]
  workdir   <-              df_in[[10]]
  #tmpdir    <-              df_in[[11]]
  savelocation    <-        df_in[[12]]
  descDate    <-              df_in[[13]]
  xlsName    <-              df_in[[14]]

  


excelFilePath<-file.path(workdir,'output',xlsName)

#set up r download as a list - information about the extractions; user-selected variables; the data
metadata <- list(title=paste("Generated ",descDate, sep=""), sql=querySent)
vdc<-list(metadata=metadata,data=df1)
save(vdc,file=excelFilePath)
#In excel, theNames will will be identifiers,and "theOutput" will hold data - set up a vector for each
theOutput <- character()
theNames <- character()
require(xlsx)
#create an excel file with 2 sheets - 1 for metadata, and 1 for data
wb <- createWorkbook()
createSheet(wb, sheetName="Metadata")
createSheet(wb, sheetName="Data")
saveWorkbook(wb, excelFilePath)

#Add more metadata about the query (other than the selected parameters), and add to start of vectors
#ensure that the ordering of "theOutput" matches "theNames"

theOutput<-c(descDate, querySent,'By default, queries are for the 30 days prior to the "date of extraction".  They can be altered to get specific date ranges, and limited to specific VRNs, hence the additional fields below.', days, startDate, endDate, vrn )
theNames<-c('Date of Extraction','SQL Used','Number of days extracted', 'NOTES:', 'Start Date (optional - default is current date)', 'End Date (optional)', 'Specific VRNs (optional)')
#we're done adding stuff, we can apply the names to the dataframe
names(theOutput)<-theNames
#write the information to the various sheets
write.xlsx(as.data.frame(theOutput), excelFilePath,sheetName="Metadata",col.names=FALSE, row.names=TRUE, append=FALSE)
write.xlsx(df1, excelFilePath,sheetName="VDC_Extraction",col.names=TRUE, row.names=FALSE, append=TRUE)
#cat(paste("<A href='http://", Sys.getenv('http_host'),"/tmp/",theFile,".xls'>Download as Excel</A>", sep=''))
file.copy(excelFilePath, file.path(savelocation,xlsName))
}


