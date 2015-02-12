

RLibrary("INLA", "numDeriv", "lubridate" )

require(PBSmapping)

# this relies upon the gsinf table which is accessible from the groundfish functions
loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 

# define location of local data files 
# net.root.dir = file.path( project.directory("groundfish"), "data", "nets" ) # eventual permanent locations
net.root.dir = file.path("C:", "Users", "MundenJ", "Desktop" )  # temporary location
 
if (FALSE) {
  scanmar.dir = file.path( net.root.dir, "Scanmar" )
  marport.dir = file.path( net.root.dir, "Marport" )
}


# steps required to recreate a local database of all data
recreate.full.database.locally = FALSE
if ( recreate.full.database.locally ) {
  # define these in your Rprofile 
  # oracle.perley.user ="username"
  # oracle.perley.password = "password"
  # oracle.perley.db = "servername"
  net_mensuration.db( DS="perley.database.datadump", net.root.dir=net.root.dir ) # ODBC data dump .. this step requires definition of password etc
  net_mensuration.db( DS="perley.database.merge", net.root.dir=net.root.dir )    # perley had two db's merge them together
  net_mensuration.db( DS="post.perley.redo",  net.root.dir=net.root.dir )        # Assimilate Scanmar files in raw data saves *.set.log files
  net_mensuration.db( DS="post.perley.merged.redo", net.root.dir=net.root.dir ) # match modern data to GSINF positions and extract Mission/trip/set ,etc
  net_mensuration.db( DS="merge.historical.scanmar.redo",  net.root.dir=net.root.dir ) # add all scanmar data together
  net_mensuration.db( DS="sanity.checks.redo",  net.root.dir=net.root.dir )      # QA/QC of data
  
  # WARNING:: the following will need to be run from inside the function 
  # as INLA does not exit gracefully from some errors
  # and R cannot catch the faults .. re-run from indicated parts 
  net_mensuration.db( DS="bottom.contact.redo",  net.root.dir=net.root.dir )  # bring in estimates of bottom contact times from scanmar
  
  net_mensuration.db( DS="scanmar.filtered.redo",  net.root.dir=net.root.dir )  # bring in estimates of bottom contact times from scanmar
 
  net_mensuration.db( DS="sweptarea.redo",  net.root.dir=net.root.dir )  
}

create.marport.database = FALSE
if (create.marport.database ) {
  net_mensuration.db( DS="marport.redo",  net.root.dir=net.root.dir )      # load data from raw data files
  net_mensuration.db( DS="marport.gated.redo",  net.root.dir=net.root.dir )      # QA/QC of data
  marport = net_mensuration.db( DS="marport.gated",  net.root.dir=net.root.dir )
}

# load master
no.matches = match.set.from.gpstrack(DS="post.perley.saved", net.root.dir=net.root.dir )
marport = net_mensuration.db( DS="marport.gated",  net.root.dir=net.root.dir )
master = net_mensuration.db( DS="sanity.checks", net.root.dir=net.root.dir )# load all scanmar data for development ...

file="master.RData"
save(master, file="Master.RData", compress=T)
load("roll.summary.RData")

# filitered to bottom contact time
filtered = net_mensuration.db( DS="scanmar.filtered",  net.root.dir=net.root.dir )  # bring in estimates of bottom contact times from scanmar

file="master.RData"
save(master, file="Master.RData", compress=T)
load("roll.summary.RData")

# fishing time total included
gs = net_mensuration.db( DS="bottom.contact",  net.root.dir=net.root.dir )  # bring in estimates of bottom contact times from scanmar
gs$ftmins = NULL
gs$ftmins = (gs$bottom_duration)/60
str(gs)
summary(gs$ftmins)
---
  # some stats for bottom contact period
gs = net_mensuration.db( DS="bottom.contact",  net.root.dir=net.root.dir )
nm = net_mensuration.db( DS="sanity.checks",  net.root.dir=net.root.dir )
nm = nm[ -which( nm$year %in% c(1990:1992) ), ]

uid = sort( unique( nm$id) )
good = NULL

for (ii in 1:length(uid))  {
  i = uid[ii]
  j = which ( gs$id == i )
  if (length(j==1)) {
    
    gg = which( nm$timestamp > gs$bc0.datetime[j] & nm$timestamp < gs$bc1.datetime[j] )
    ss = length(gg) 
    if (ss < 1) next()
    if (ss > 30 &&  is.finite(gs$bc0.sd[j]) && is.finite(gs$bc1.sd[j]) && gs$bc0.sd[j] < 30 &&  gs$bc1.sd[j] < 30 ) {
      good = c(good, gg)
      print (paste( ii, i, ss ))    
    }
  }

}
  

nm.filtered = nm[good,]


----

file="basedata"
save(basedata, file="basedata.RData", compress=T)
load("basedata.RData")


--- testing / development ---

no.matches = net_mensuration.db( DS="post.perley.merged", net.root.dir=net.root.dir )
marport = net_mensuration.db( DS="marport.gated",  net.root.dir=net.root.dir )
master = net_mensuration.db( DS="sanity.checks", net.root.dir=net.root.dir )# load all scanmar data for development ...

# load gsinf
gsinf = groundfish.db( DS="gsinf" )

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


allids=unique(master$id)
i=sample(1:length(allids),15)
allids=allids[i]
allids
  id="TEL2004530.99"
  mm = master[ which(master$year== 2006) , ]
  plot(depth~timestamp, mm, main= "TEL2004529.21")
  
length(unique(mm$latitude))
length(unique(mm$longitude))  
head(mm$latitude)  
summary(mm$latitude)
tail(mm$latitude)

r.samp=filteredlm[sample(1:nrow(filteredlm), 10, replace=FALSE),]
r.samp$id
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
    
    id = "NED2005001.1" 
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
net_mensuration.db( DS="bottom.contact.redo", net.root.dir=net.root.dir, user.interaction=FALSE, override.missions=mission.list  )



# General depth plots
i = master[which(master$id == "NED2014002.38"), ]
d=range(i$depth, na.rm=TRUE)
plot(depth~timestamp,i, type = "p", lwd = 1, main =  "Depth", sub=id, ylim=c(d[2], d[1]))
