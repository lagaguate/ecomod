
p = list()

p$libs = RLibrary("INLA", "numDeriv", "lubridate" )

# this relies upon the gsinf table which is accessible from the groundfish functions
p$inits = loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 

# define location of local data files 
p$scanmar.dir = file.path( project.directory("groundfish"), "data", "nets", "Scanmar" ) 
p$marport.dir = file.path( project.directory("groundfish"), "data", "nets", "Marport" ) 

p$current.year = 2014
p$netmensuration.years = c(1990:1992, 2004:p$current.year)  ## 2009 is the first year with set logs from scanmar available .. if more are found, alter this date
p$netmensuration.years = p$current.year  ## for incremental/annual update




# steps required to recreate a local database of all data
recreate.perley.db = FALSE
if ( recreate.perley.db ) {
  # define these in your Rprofile 
  # oracle.perley.user ="username"
  # oracle.perley.password = "password"
  # oracle.perley.db = "servername"
  scanmar.db( DS="perley.datadump", p=p ) # ODBC data dump .. this step requires definition of password etc
  scanmar.db( DS="perley.redo", p=p )    # perley had two db's merge them together: from XXXX-2002 and 2006 to 200X
}


# the following works upon annual time slices ( defined in p$netmensuration.years )

  scanmar.db( DS="basedata.redo", p=p )        # Assimilate Scanmar files in raw data saves *.set.log files
  scanmar.db( DS="basedata.lookuptable.redo", p=p ) # match modern data to GSINF positions and extract Mission/trip/set ,etc
  scanmar.db( DS="sanity.checks.redo",  p=p )      # QA/QC of data
  
  # WARNING:: the following may crash as INLA does not exit gracefully from some errors
  # and R cannot catch the faults .. restart R or reboot the system (it can happen) 
  # and then re-run the line and it will continue from where it crashed
  scanmar.db( DS="bottom.contact.redo",  p=p )  # bring in estimates of bottom contact times from scanmar
  scanmar.db( DS="sweptarea.redo",  p=p )  



create.marport.database = FALSE
if (create.marport.database ) {
  marport.db( DS="marport.redo",  p=p )      # load data from raw data files
  marport.db( DS="marport.gated.redo",  p=p )      # QA/QC of data
  marport = marport.db( DS="marport.gated",  p=p )
}



## -------------- local analysis

require(PBSmapping)
# load master
no.matches = scanmar.db( DS="post.perley.saved", p=p )
master = scanmar.db( DS="sanity.checks", p=p )# load all scanmar data for development ...
marport = marport.db( DS="marport.gated",  p=p )

file="master.RData"
save(master, file="Master.RData", compress=T)
load("roll.summary.RData")

# filitered to bottom contact time
filtered = scanmar.db( DS="scanmar.filtered",  p=p )  # bring in estimates of bottom contact times from scanmar

file="master.RData"
save(master, file="Master.RData", compress=T)
load("roll.summary.RData")

# fishing time total included
gs = scanmar.db( DS="bottom.contact",  p=p )  # bring in estimates of bottom contact times from scanmar
gs$ftmins = NULL
gs$ftmins = (gs$bottom_duration)/60
str(gs)
summary(gs$ftmins)
---
  # some stats for bottom contact period
gs = scanmar.db( DS="bottom.contact",  p=p )
nm = scanmar.db( DS="sanity.checks",  p=p )
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

no.matches = scanmar.db( DS="post.perley.merged", p=p )
marport = marport.db( DS="marport.gated",  p=p )
master = scanmar.db( DS="sanity.checks", p=p )# load all scanmar data for development ...

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


# General depth plots
i = master[which(master$id == "NED2014002.38"), ]
d=range(i$depth, na.rm=TRUE)
plot(depth~timestamp,i, type = "p", lwd = 1, main =  "Depth", sub=id, ylim=c(d[2], d[1]))
