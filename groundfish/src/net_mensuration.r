
library(lubridate)

# this relies upon the gsinf table which is accessible from the groundfish functions
loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 

# define location of local data files
netswd = file.path("C:", "Users", "MundenJ", "Desktop", "Scanmar")
marportdatadirectory = file.path("C:", "Users", "MundenJ", "Desktop", "Marport", "Logs")

# steps required to recreate a local database of all data
recreate.full.database.locally = FALSE
if ( recreate.full.database.locally ) {
  # define these in your Rprofile 
  # oracle.perley.user ="username"
  # oracle.perley.password = "password"
  # oracle.perley.db = "servername"
  net_mensuration.db( "perley.database.datadump", netswd ) # ODBC data dump .. this step requires definition of password etc
  net_mensuration.db( "perley.database.merge", netswd )    # perley had two db's merge them together
  net_mensuration.db( "post.perley.redo",  netswd )        # Assimilate Scanmar files in raw data saves *.set.log files
  match.set.from.gpstrack(DS="post.perley.redo", netswd=netswd ) # match modern data to GSINF positions and extract Mission/trip/set ,etc
  net_mensuration.db( "merge.historical.scanmar.redo",  netswd ) # add all scanmar data together
  net_mensuration.db( "sanity.checks.redo",  netswd )      # QA/QC of data
}

no.matches = match.set.from.gpstrack(DS="post.perley.saved", netswd=netswd )

# load all scanmar data for development ...
master = net_mensuration.db( DS="sanity.checks", netswd=netswd )

i = which(is.na(master$id))
t = unique( master$netmensurationfilename[i])
p = data.frame(id = t)
write.table(t, file= "missing_id.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)

--- testing / development ---

# Adding the variables: year, trip and set to the df master
master$date=substring(master$timestamp,0,9)  
master$year=as.numeric(substring(master$id,4,7))
master$trip=as.numeric(substring(master$id,8,10))
master$set=as.numeric(substring(master$id,12,14))
# Producing a version of master that includes the historical data
modern.data=master[which(master$year %in% 2004:2014) , ]

# Only run to genereate new samples
allids=unique(modern.data$id)
i=sample(1:length(allids),5)
allids=allids[i]
allids

  # Run for many sets
  for (id in allids){
    test = which(modern.data$id==id)
    mm = modern.data[test, ]
    
    # Run for one set
    id = "TEM2008830.115"
    mm = master[ which(master$id==id),]
    
    # Ran in both cases
      bc = NULL
      bc = bottom.contact.groundfish(mm,  depthproportion=0.5, nbins=c(5,10) , minval.modal=5 ) 
           
max(bc$filtered.data$depth, na.rm=TRUE)
sd(bc$filtered.data$depth, na.rm=TRUE)
str(bc)
summary(mm$depth)    
            

summary(x$depth)
summary(x$timestamp)
diff(bc$smooth.method)
diff(bc$linear.method)
diff(bc$modal.method)
str(bc)

rawdata = master[ which(master$id==i),]
plot(depth~timestamp, rawdata)
plot(depth~timestamp, rawdata, ylim=c(250,0))

points(x$timestamp[bc$variance.method.indices],  x$depth[bc$variance.method.indices], col="violet", pch=19)




