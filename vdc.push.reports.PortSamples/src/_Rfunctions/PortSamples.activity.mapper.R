PortSamples.activity.mapper<-function(
  #default values
#   dsn         = oracle.dsn,
#   user        = oracle.PortSamples.user,
#   pw          = oracle.PortSamples.password,
  dsn="PTRAN",
  user="mflib",
  pw="FH6EIO",
  debug       = F, 
  last_n_days = 5,                          
  startdate   = NULL, 
  enddate     = NULL,
  vessel_list = c(),
  filename    = "Groundfish_MG_65_ITQ",  #what the file will be called (plus timestamp)
  title       = "Groundfish MG &#60; 65' ITQ"   #title at top of kmz   
){

options(stringsAsFactors=F)
options(warn=-1)
workdir <- file.path(project.directory('vdc.push.reports.portsamples'),"src" )
setwd(workdir)
savelocation<- file.path( workdir,"output")
map.vessels<-function(dsn, user, pw, last_n_days, vessel_list, filename, title){
    if (exists("dfKeep")&&debug==T){
      cat("-------> Using existing data <--------\n")
      use.df<-dfKeep
    }else{
      cat("Getting new data...\n")
      use.df<-PortSamples.get.data(dsn, user, pw, last_n_days, startdate, enddate, vessel_list)
    }
    the.df<-c(use.df,filename,title, workdir )
    PortSamples.make.kml(the.df)
  }
map.vessels(dsn, user, pw, last_n_days, vessel_list, filename, title)    
}