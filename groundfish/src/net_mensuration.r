

RLibrary("INLA", "numDeriv", "lubridate" )

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
  net_mensuration.db( DS="bottom.contact.redo",  netswd=netswd )  # bring in estimates of bottom contact times from scanmar
  net_mensuration.db( DS="sweptarea.redo",  netswd=netswd )  
  
}

no.matches = match.set.from.gpstrack(DS="post.perley.saved", netswd=netswd )

# load marport data
net_mensuration.db( DS="marport.redo",  netswd=marportdatadirectory ) # load data
net_mensuration.db( DS="marport.gated.redo",  netswd=marportdatadirectory ) # QA/QC of data
marport = net_mensuration.db( DS="marport.gated",  netswd=marportdatadirectory ) # Not working need to fix to load marport data
str(marport)


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
allids=unique(master$id)
i=sample(1:length(allids),15)
allids=allids[i]
allids
  id="NED2013028.115"
  mm = master[ which(master$id==id) , ]
  plot(depth~timestamp, mm, main= id)
  
  
  
# how many unique sets were in 2013/2014
test=master[which(master$year == 2013) , ]  
length(unique(test$id))  
# 122 sets in 2014
# 248 sets in 2013
122+248

# Adding the variables: year, trip and set to the df master
master$date=substring(master$timestamp,0,10)  

# Only run to genereate new samples
allids=unique(modern.data.2006$id)
i=sample(1:length(allids),15)
allids=allids[i]
allids

modern.data.2004=modern.data[which(modern.data$year ==2004) , ]
modern.data.2005=modern.data[which(modern.data$year ==2005) , ]
modern.data.2006=modern.data[which(modern.data$year ==2006) , ]




  # Run for many sets
  for (id in allids){
    
    id = "NED2006030.73" 
    mm = modern.data[ which(modern.data$id==id) , ]
   # mm = master[ which(master$id==id),]
      bc = NULL
      bc = bottom.contact(id, mm, depthproportion=0.6, tdif.min=15, tdif.max=45, eps.depth=3, sd.multiplier=4, depth.min=10, depth.range=50, smoothing = 0.9, filter.quants=c(0.025, 0.975), plot.data=TRUE) 
    
           
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




