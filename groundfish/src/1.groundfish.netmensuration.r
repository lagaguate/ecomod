
p = list()
p$libs = RLibrary("INLA", "numDeriv", "lubridate", "geosphere" )
# this relies upon the gsinf table which is accessible from the groundfish functions
p$inits = loadfunctions( "groundfish", functionname="load.groundfish.environment.r")
# define location of local data files
p$scanmar.dir = file.path( project.datadirectory("groundfish"), "data", "nets", "Scanmar" )
p$marport.dir = file.path( project.datadirectory("groundfish"), "data", "nets", "Marport" )



# pick the year to process:
# p$netmensuration.years = p$current.year  ## for incremental/annual update
# 2009 is the first year with set logs from scanmar available .. if more are found, alter this date
p$current.year = 2015
p$netmensuration.years = c(1990:1992, 2004:p$current.year) # NOTE:: 1990 to 1992 data really do not match the timestamps (and have no location info)

# p$netmensuration.years = p$current.year

# two depth sensors were used simultaneously but they are not calibrated!
# they are remarkably hard to filter out while still maintaining current methods
# instead: send a trigger to bottom.contact to operate on this properly
p$id.double.depth.sensors = paste( "NED2015002", c( 51:54, 55:64), sep="." )
# set id's that should be skipped
p$problem.sets = c("NED2014018.27")


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


# the following works upon many or annual time slices ( defined in p$netmensuration.years )
scanmar.db( DS="basedata.redo", p=p )        # Assimilate Scanmar files in raw data saves *.set.log files
scanmar.db( DS="basedata.lookuptable.redo", p=p ) # match modern data to GSINF positions and extract Mission/trip/set ,etc
scanmar.db( DS="sanity.checks.redo",  p=p )      # QA/QC of data


# WARNING:: the following may crash as INLA does not exit gracefully from some errors
# and R cannot catch the faults .. restart R or reboot the system (it can happen)
# doing it year by year is probably wise for now
# usually insufficient data for these or just flat-lines .. no reliable data

if (FALSE) {
  # simple:
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "TEL2004529.1") # simple, low n
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "TEL2004529.47")
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2010027.139" ) # n=1851 ??  smooth get chopped up?
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2015002.27") # normal .. lots of data
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2015002.55") # ???

  # challenging tests:
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "TEL2004529.20") # no tail, humped
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "TEL2004529.16") # very large range
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2013028.10") # simple, n=1663, strange tail
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2015002.12") # missing a tail
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "TEL2004530.84") # missing a tail

  # no data?
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2015002.59") # two depth sensors  in the same log ! --
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2015002.64") # two depth sensors  --
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2014018.27")  ## a lot of data ???

  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "TEL2004529.11")  ## a lot of data ???




  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "TEL2004530.84")  ## a lot of data ???
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "TEL2004530.85")  ## a lot of data ???

  # to  load a single result and view
  # bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2010027.53") # strange wingspreads
  # bottom.contact.plot( bc, netspread=TRUE )
}



scanmar.db( DS="bottom.contact.redo",  p=p )  # bring in estimates of bottom contact times from scanmar

# swept areas are computed in bottom.contact.redo ..
# this step estimates swept area for those where there was insufficient data to compute SA directly from logs,
# estimate via approximation using speed etc.
scanmar.db( DS="sweptarea.redo",  p=p )

figures.netmensuration( DS="all", p=p )


# netmind base data filtered for fishing periods .. not really used except for some plots
scanmar.db( DS="scanmar.filtered.redo",  p=p )

create.marport.database = FALSE
if (create.marport.database ) {
  p$netmensuration.years = 2013:2014
  marport.db( DS="basedata.redo",  p=p )      # load data from raw data files
  marport.db( DS="gated.redo",  p=p )      # QA/QC of data
  marport = marport.db( DS="gated",  p=p )
}









# --- misc stats / analysis

gs0 = scanmar.db( DS="bottom.contact", p=p )  # bring in estimates of bottom contact times from scanmar



gs0$timediff_official =  as.numeric(gs0$edate - gs0$sdate) / 60 # that which would have been used (if at all .. )
gs0$bottom.dist = geosphere::distGeo( gs0[, c("bc.lon0","bc.lat0")], gs0[, c("bc.lon1", "bc.lat1")])/1000


# settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage),
        #  4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment,
        #  6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography

# setype
table(gs0$settype)
   1    3    4    5    8    9
5333  294    1  608  147   59


# settype vs mean duration
tapply( gs0$bottom_duration/60, gs0$settype, mean, na.rm=TRUE )
       1        3        4        5        8        9
29.76567      NaN      NaN 28.45052      NaN      NaN



w2a = which( gs0$geardesc == "Western IIA trawl" & gs0$settype %in% c(1,2,5) )     # for distribution checks for western IIA trawl
gs = gs0[ w2a, ]

# trawl duration bias
plot( jitter(gs$timediff_official), jitter(gs$bottom_duration/60), xlim=c(10,40), cex=0.65, col="slateblue",
     xlab="Trawl duration: official (minutes)", ylab="Trawl duration: computed (minutes)" )
abline(a=0,b=1, col="grey" )


# distance bias
plot( jitter(gs$dist_km), jitter(gs$bottom.dist), xlim=c(1.5,4),ylim=c(0,5),cex=0.65, col="slateblue",
     xlab="Trawl length: official (km)", ylab="Trawl length: computed from end points (km)" )
abline(a=0,b=1, col="grey" )


# distance bias
plot( jitter(gs$bc.dist), jitter(gs$bottom.dist), xlim=c(1.5,6),ylim=c(0,6),cex=0.65, col="slateblue",
     xlab="Trawl length: official (km)", ylab="Trawl length: computed from track(km)" )
abline(a=0,b=1, col="grey" )


# distance bias ( vert has no influence)
plot( jitter(gs$bc.dist), jitter(gs$bc.dist.h), xlim=c(1.5,4),ylim=c(0,5),cex=0.65, col="slateblue",
     xlab="Trawl length: total (km)", ylab="Trawl length: horiz (km)" )
abline(a=0,b=1, col="grey" )




# swept area (door width X distance)
plot( door.sa ~ as.factor(yr), gs[ gs$yr> 2009,], ylab="Door width (m)", xlab="Year" )

plot( door.sa ~ log(bottom_depth), gs[ ,], xlim=log(c(20,650)) )
> plot( door.sa ~ log(bottom_depth), gs[ ,], xlim=log(c(20,650)) )
>
# Swept area vs depth
plot( (door.sa) ~ log(bc.depth.mean), gs[ ,], col="slategray", cex=0.5, xlab="log Depth (m)", ylab="Swept area (km^2)" )

# points( (door.sa) ~ log( bc.depth.mean), gs[gs$yr==2010 ,], pch="*", col="red" )
points( (door.sa) ~ log( bc.depth.mean), gs[gs$yr==2011 ,], pch=20, col="steelblue", cex=1.25 )
#points( (door.sa) ~ log( bc.depth.mean), gs[gs$yr==2012 ,], pch=22, col="orange" )
#points( (door.sa) ~ log( bc.depth.mean), gs[gs$yr==2013 ,], pch=19, col="brown", cex= 0.8 )
#points( (door.sa) ~ log( bc.depth.mean), gs[gs$yr==2014 ,], pch=19, col="brown", cex= 0.8 )
#points( (door.sa) ~ log( bc.depth.mean), gs[gs$yr==2015 ,], pch=19, col="red", cex= 0.8 )


plot( bottom_duration ~ as.factor( strat), gs[ gs$yr> 2008,] )
plot( bottom_duration ~ bottom_salinity, gs[ ,], xlim=c(31,36) )
plot( door.sa ~ bottom_salinity, gs[ ,], xlim=c(31,36) )
plot( door.sa ~ bottom_depth, gs[ ,], xlim=c(31,36) )

plot( door.sa ~ log(bottom_depth), gs[ ,], xlim=log(c(20,650)) )




plot( bottom_duration ~ as.factor(yr), gs[ gs$yr> 2008,] )

plot( (bottom_duration) ~ bc.depth.mean, gs[ ,], pch=".", col="red" )
points( (bottom_duration) ~ bc.depth.mean, gs[gs$yr==2010 ,], pch="*", col="red" )
points( (bottom_duration) ~ bc.depth.mean, gs[gs$yr==2011 ,], pch="*", col="blue" )
points( (bottom_duration) ~ bc.depth.mean, gs[gs$yr==2012 ,], pch=22, col="orange" )
points( (bottom_duration) ~ bc.depth.mean, gs[gs$yr==2013 ,], pch=19, col="brown", cex= 0.8 )
points( (bottom_duration) ~ bc.depth.mean, gs[gs$yr==2014 ,], pch=19, col="brown", cex= 0.8 )
points( (bottom_duration) ~ bc.depth.mean, gs[gs$yr==2015 ,], pch=19, col="red", cex= 0.8 )








pp = tapply( gs$id, year(gs$bc0.datetime), function(x) { length(unique(x))} )
pp = data.frame( yr= rownames(pp), n.bc=pp)
#scanmar.db( DS="bottom.contact.redo",  p=p )  # bring in estimates of bottom contact times from scanmar

oo = tapply( gs$id, year(gs$sdate), function(x) { length(unique(x))} )
oo = data.frame( yr = rownames(oo), n.gs= oo)
length( gs[ which(is.finite(gs$bottom_duration) ), "id" ] )
length( gs[ which(is.finite(gs$bottom_duration) & gs$bc0.sd<30 & gs$bc1.sd<30), "id" ] )


sc = scanmar.db( DS="sanity.checks",  p=p )      # QA/QC of data
rr = tapply( sc$id, sc$year, function(x) { length(unique(x))} )
rr = data.frame( yr= rownames(rr), n.scanmar=rr )

nm = scanmar.db( DS="scanmar.filtered",  p=p )  # bring in estimates of bottom contact times from scanmar
qq = tapply( nm$id, nm$year, function(x) { length(unique(x))} )
qq = data.frame( yr= rownames(qq), n.filtered=qq )


res = merge ( oo, pp, by="yr")
res = merge ( res, qq, by="yr")
res = merge ( res, rr, by="yr")





# ----
# debugging SA estimates

g = scanmar.db( DS="bottom.contact",  p=p )
plot( dist_km~ I(wing.sa / wing.mean*1000), g, ylim=c(0,5) )
gr = abs(g$dist_km - (g$wing.sa / g$wing.mean)*1000)
strange = which ( gr > 1 & g$gear==9 & g$settype==1 )
g[strange, "id"]

"NED2014101.23" "NED2011002.6"  "NED2014101.13" "NED2014101.15" "NED2015002.20"

bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2014018.71")  # depth sensor not working
bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2013022.192") # large depth range
bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2013022.205") # depth sensor not working
bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2013022.208") # GPS not working
bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2011002.53")  #
bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2011002.45")  # doorspread failure and almost no wingspread
bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2014101.23")
bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2011002.6")


bottom.contact.plot( bc, netspread=TRUE )


bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2011002.45") # GPS not working
bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2013022.208") # GPS not working

bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2009002.17")  # doorspread failure and almost no wingspread


bottom.contact.plot( bc, netspread=TRUE )
plot( longitude ~ latitude, bc$plotdata )
plot( wingspread ~ ts, bc$plotdata )
plot( doorspread ~ ts, bc$plotdata )

uu = scanmar.db( DS="sanity.checks",  p=p, YR=2011 )
uu = scanmar.db( DS="basedata", p=p, YR=2011 )        # Assimilate Scanmar files in raw data saves *.set.log files
vv = scanmar.db( DS="basedata.lookuptable", p=p )
ww = which( vv$id=="NED2009027.25" )
xx = uu[ which( uu$nm_id == vv$nm_id[ww] ) , ]
plot( wingspread ~ timestamp, xx )
plot( doorspread ~ timestamp, xx )



