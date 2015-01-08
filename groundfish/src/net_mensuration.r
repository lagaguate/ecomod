

RLibrary( "lubridate", "INLA", "numDeriv" )

# this relies upon the gsinf table which is accessible from the groundfish functions
loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 

# define location of local data files
netswd = file.path("C:", "Users", "MundenJ", "Desktop", "Scanmar")
# netswd = "~/Downloads"
# load( "~/Downloads/m.data.RData")

marportdatadirectory = file.path("C:", "Users", "MundenJ", "Desktop", "Marport")
# netswd = marportdatadirectory


# steps required to recreate a local database of all data
recreate.full.database.locally = FALSE
if ( recreate.full.database.locally ) {
  # define these in your Rprofile 
  # oracle.perley.user ="username"
  # oracle.perley.password = "password"
  # oracle.perley.db = "servername"
  net_mensuration.db( DS="perley.database.datadump", netswd=netswd ) # ODBC data dump .. this step requires definition of password etc
  net_mensuration.db( DS="perley.database.merge", netswd=netswd )    # perley had two db's merge them together
  net_mensuration.db( DS="post.perley.redo",  netswd=netswd )        # Assimilate Scanmar files in raw data saves *.set.log files
  match.set.from.gpstrack(DS="post.perley.redo", netswd=netswd ) # match modern data to GSINF positions and extract Mission/trip/set ,etc
  net_mensuration.db( DS="merge.historical.scanmar.redo",  netswd=netswd ) # add all scanmar data together
  net_mensuration.db( DS="sanity.checks.redo",  netswd=netswd )      # QA/QC of data
  net_mensuration.db( DS="marport.redo",  netswd=marportdatadirectory )      # QA/QC of data
}

no.matches = match.set.from.gpstrack(DS="post.perley.saved", netswd=netswd )

# load marport data
marport = net_mensuration.db( DS="marport",  netswd=marportdatadirectory )      # QA/QC of data

# load all scanmar data for development ...
master = net_mensuration.db( DS="sanity.checks", netswd=netswd )

# Load marport/basedata
load("C:/Users/mundenj/Desktop/Marport/marport.rdata")

i = which(is.na(master$id))
t = unique( master$netmensurationfilename[i])
p = data.frame(id = t)
write.table(t, file= "missing_id.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)

# Saving local copies of historical and modern data
historical.data=master[which(master$year %in% 1990:1992) , ]
file="h.data.RData"
save(historical.data, file="h.data.RData", compress=T)
modern.data=master[which(master$year %in% 2004:2014) , ]
file="m.data.RData"
save(modern.data, file="m.data.RData", compress=T)
# Load copies for current session
load("h.data.RData")
load("m.data.RData")

--- testing / development ---

# Adding the variables: year, trip and set to the df master
master$date=substring(master$timestamp,0,10)  

# Only run to genereate new samples
allids=unique(modern.data$id)
i=sample(1:length(allids),15)
allids=allids[i]
allids

  # Run for many sets
  for (id in allids){
    test = which(modern.data$id==id)
    mm = modern.data[test, ]
    
    # Run for one set
    # id = "NED2010027.225" fail but plots
    id =  "NED2009027.108"
    id =  "NED2011002.74"
    id = "NED2010027.189"
    id = "TEM2008830.141"
    id = "TEL2007745.23" 
    id = "TEL2004530.16"
    
    mm = master[ which(master$id==id),]
    
    # to load/save
    # fname = "mm.rdata"
    # save( mm, file=fname, compress=TRUE)
    # load( fname )
    
    # Ran in both cases
      bc = NULL
      bc = bottom.contact(id, mm, depthproportion=0.6, tdif.min=15, tdif.max=45, eps.depth=3, sd.multiplier=seq( 3, 1, by=-0.1),smoothing = 0.9, filter.quants=c(0.025, 0.975), plot.data=TRUE) 
    
           
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


# Only run to genereate new samples
loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 
allids=unique(modern.data$id)

i=sample(1:length(allids),5)
mission.list=allids[i]
mission.list
net_mensuration.db( DS="bottom.contact.redo", netswd=netswd, user.interaction=FALSE, override.missions=mission.list  )




