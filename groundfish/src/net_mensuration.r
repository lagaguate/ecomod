
p = list()
p$libs = RLibrary("INLA", "numDeriv", "lubridate", "geosphere" )
# this relies upon the gsinf table which is accessible from the groundfish functions
p$inits = loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 
# define location of local data files 
p$scanmar.dir = file.path( project.datadirectory("groundfish"), "data", "nets", "Scanmar" ) 
p$marport.dir = file.path( project.datadirectory("groundfish"), "data", "nets", "Marport" ) 



# pick the year to process:
# p$netmensuration.years = p$current.year  ## for incremental/annual update
## 2009 is the first year with set logs from scanmar available .. if more are found, alter this date
p$current.year = 2015
p$netmensuration.years = c(1990:1992, 2004:p$current.year) # NOTE:: 1990 to 1992 data really do not match the timestamps (and have no location info) 

# p$netmensuration.years = p$current.year  

# two depth sensors were used simultaneously but they are not calibrated!
# they are remarkably hard to filter out while still maintaining current methods
# instead: send a trigger to bottom.contact to operate on this properly
p$double.depth.sensors = paste( "NED2015002", c( 51:54, 55:64), sep="." )


# the data for these sets need to be checked?
p$bc.badlist = c( 
  "NED2012002.17", "TEL2005545.73","NED2015002.7", "NED2015002.8", "NED2015002.9"
) 



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
# and then re-run the line and it will continue from where it crashed ... update the bc.badlist too
# doing it year by year is probably wise for now
# usually insufficient data for these or just flat-lines .. no reliable data

if (FALSE) { 
  # tests of extreme data conditions :
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "TEL2004529.1") # simple, low n 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "TEL2004530.21") # simple, low n 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "TEL2004530.41") # simple, low n 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "TEL2004529.59") # simple, low n 

  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "TEL2004530.46") # complex hump, low n 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "TEL2004530.70") # simple, low n 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2010027.139" ) # n=1851
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2013022.192")  ### large depth range and wrong
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2013028.10") # simple, n=1663, strange tail 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2014102.35") # constant depth data ...  --
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2015002.12") # missing a tail 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2015002.27") # normal .. lots of data 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2015002.59") # two depth sensors  --
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2015002.64") # two depth sensors  --

  # to  load a single result and view
  bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2012002.97") #  strange wingspreads
  bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2010027.100") #  strange wingspreads ... esp in 210 :: NED2010027.xxx -- looks like wingspread did not function properly!!
  bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2010027.53") # strange wingspreads

  bottom.contact.plot( bc, netspread=TRUE )
}

scanmar.db( DS="bottom.contact.redo",  p=p )  # bring in estimates of bottom contact times from scanmar

# swept areas are computed in bottom.contact.redo .. 
# this step estimates swept area for those where there was insufficient data to compute SA directly from logs, 
# estimate via approximation using speed etc. 
scanmar.db( DS="sweptarea.redo",  p=p ) 

# netmind base data filtered for fishing periods .. not really used except for some plots
scanmar.db( DS="scanmar.filtered.redo",  p=p )  

create.marport.database = FALSE
if (create.marport.database ) {
  p$netmensuration.years = 2013:2014  
  marport.db( DS="basedata.redo",  p=p )      # load data from raw data files
  marport.db( DS="gated.redo",  p=p )      # QA/QC of data
  marport = marport.db( DS="gated",  p=p )
}




### -----------------------------------------------------------------------
### -- example usage directly upon a single file, perhaps on board/in situ
### -----------------------------------------------------------------------

# file list can be created and then a gui can be 
# created to use mouse to choose file and run etc ... be my guest .. :)

RLibrary( "lubridate", "sp", "INLA", "Matrix", "splines" ) 
loadfunctions( "utility" )
loadfunctions( "groundfish", "netmensuration")
datadir = file.path( project.datadirectory("groundfish"), "data", "nets", "Scanmar", "datalogs", "2015", "NED2015002" )  # storage location
fn = "NED2015002.028.2015-Mar21-162742.SET.LOG" # filename of data file to examine
fl = file.path( datadir, fn)
mm = load.scanmar.rawdata( fl ) 
bcp = list( id=fn, datasource="groundfish", nr=nrow(mm), tdif.min=9, tdif.max=45, user.interaction=TRUE )  ### yes some are as short as 9 min .. turn user interaction off if you would like automatic solutions only
bcp = bottom.contact.parameters( bcp ) # add other default parameters
bc =  bottom.contact(mm, bcp )
bottom.contact.plot( bc, netspread=TRUE )
str(bc)





# --- misc stats / analysis

gs = scanmar.db( DS="bottom.contact",  p=p )  # bring in estimates of bottom contact times from scanmar
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
plot( I(dist*1.78) ~ I(wing.sa / wing.mean*1000), g)
gr = abs(g$dist*1.78 - (g$wing.sa / g$wing.mean)*1000) 
strange = which ( gr > 1 & g$gear==9 & g$settype==1 )
g[strange, "id"]
 
 [1] "NED2009027.117" "NED2009027.124" "NED2009027.151" "NED2009027.153" "NED2009027.177" "NED2009027.3"  
 [7] "NED2009027.36"  "NED2009027.43"  "NED2009027.52"  "NED2009027.62"  "NED2009027.93"  "NED2010027.219"
[13] "NED2011002.9"   "NED2011002.44"  "NED2011002.45"  "NED2011002.53"  "NED2013022.205" "NED2013022.192"

bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2014018.71")  # depth sensor not working
bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2013022.192") # large depth range
bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2013022.205") # depth sensor not working
bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2013022.208") # GPS not working
bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2011002.53")  # 
bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2011002.45")  # doorspread failure and almost no wingspread 
bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2009027.26") 
bottom.contact.plot( bc, netspread=TRUE )


bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2011002.45") # GPS not working
bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2013022.208") # GPS not working

bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2012002.98")  # doorspread failure and almost no wingspread 


bottom.contact.plot( bc, netspread=TRUE )
plot( longitude ~ latitude, bc$plotdata )
plot( wingspread ~ ts, bc$plotdata )
plot( doorspread ~ ts, bc$plotdata )

uu = scanmar.db( DS="sanity.checks",  p=p, YR=2011 )      
uu = scanmar.db( DS="basedata", p=p, YR=2011 )        # Assimilate Scanmar files in raw data saves *.set.log files
vv = scanmar.db( DS="basedata.lookuptable", p=p )
ww = which( vv$id=="NED2011002.45" )
xx = uu[ which( uu$nm_id == vv$nm_id[ww] ) , ]
plot( wingspread ~ timestamp, xx )
plot( doorspread ~ timestamp, xx )



