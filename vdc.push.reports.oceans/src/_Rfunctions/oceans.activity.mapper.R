oceans.activity.mapper<-function(
  debug       = T,                           
  last_n_days = 30,                          
  startdate   = NULL, 
  enddate     = NULL,
  vessel_list = c(),          
  datawindows = c("VazellaEmerald")
  ){
  #Housekeeping

options(stringsAsFactors=F)
options(warn=-1)
workdir     <- file.path( "C:","Users","mcmahonm","Documents","GitHub","ecomod","vdc.push.reports.oceans","src" )
setwd(workdir)
tmpdir      <- file.path( workdir,"tmp" )
savelocation<- file.path( workdir,"output")
#reference other files that are required
cat("Loading required files\n")
#source("Oceans/oceans.get.data.R")
#source("Oceans/oceans.make.kml.R")
#source("Oceans/oceans.send.email.R")
################################################
#Place to alter report parameters
#NOTE regarding date specifity:
#if neither start or end date is entered, data is returned relative to NOW
#if both dates are entered, data is returned for that range
#if one date is entered, data is returned for x days prior to that date

#debug       = T                           #"T" reuses last data-grab, "F" gets new data
#last_n_days = 30                            #previous x days (eg 30)
#startdate   = NULL            #Local startdate (eg '2013-10-27 16:00')
#enddate     = NULL                          #Local enddate (eg '2013-11-27 05:00')  
#vessel_list = c()                       #vector of vrn (eg c(877718,102339) )
#datawindows = c("Lophelia CCA","Northeast Channel","Gully","VazellaEmerald","St Anns Bank Inventory Box","Musquash")
#datawindows = c("VazellaEmerald")

#used - c("Lophelia CCA","Northeast Channel","Gully","VazellaEmerald","St Anns Bank Inventory Box","Musquash")
#other - c("Russian Hat Assessment Box1", "Stone Fence","Haddock Closed Area", "roseway whale","Fundy Whale Sanctuary")
	#"All" - "All" gets one massive file of all areas - just like the old (wrong) NE channel report
    #"Haldimand Canyon" and "Shortland Canyon" don't have data windows so can't be generated.

################################################
map.vessels<-function(datawindows, last_n_days, startDate, endDate, vessel_list,workdir,tmpdir,savelocation){
      if (exists("dfKeep")&&debug==T){
        cat("-------> Using existing data <--------\n")
        setwd(workdir)
        the.df<-dfKeep
        the.df<-c(the.df,workdir,tmpdir,savelocation )
        results1<-oceans.make.kml(the.df)
		cat("Send Emails\n")
        oceans.send.email(results1,workdir,debug)
      }else{
        cat("Getting new data...\n")
        for (i in 1:length(datawindows)){
          cat(paste("starting ", datawindows[i], "\n"))
          setwd(workdir)
          the.df<-oceans.get.data(datawindows[i], last_n_days, startdate, enddate, vessel_list)
          the.df<-c(the.df,workdir, tmpdir, savelocation )
          results1<-oceans.make.kml(the.df)
		  cat("Send Emails\n")
          oceans.send.email(results1,workdir,debug)
          if (length(datawindows>1)){
            cat(paste("Finished ", datawindows[i],"\n"))
          }
          cat("Finished Request\n")
        }
      }
}
map.vessels(datawindows, last_n_days, startdate, enddate, vessel_list,workdir,tmpdir,savelocation)
cat("Done...\n")
}