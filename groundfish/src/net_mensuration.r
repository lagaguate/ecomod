
library(lubridate)
# this relies upon the gsinf table which is accessible from the groundfish functions
loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 
# define a few file locations
# file path should be altered once I get my own log on
netswd = file.path("C:", "Users", "MundenJ", "Desktop", "Scanmar")
nmfunctionsdirectory = file.path(netswd, "net_mensuration")
netmensuration.files = list.files( nmfunctionsdirectory, full.names=TRUE )
for (fn in netmensuration.files) source(fn)

master = net_mensuration.db( DS="sanity.checks", netswd=netswd )


# Producing a version of master that includes the historical data
master$year=as.numeric(substring(master$id,4,7))
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
    id = "NED2009027.139"
    mm = master[ which(master$id==id),]
    
    # Ran in both cases
      bc = NULL
      bc = bottom.contact.groundfish(mm,  depthproportion=0.9, nbins=c(5,10) , minval.modal=5 ) 
           
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




