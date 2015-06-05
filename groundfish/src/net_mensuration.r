
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

# two depth sensors were used simultaneously but they are not calibrated!
# they are remarkably hard to filter out while still maintaining current methods
# instead: send a trigger to bottom.contact to operate on this properly
p$double.depth.sensors = paste( "NED2015002", c( 51:54, 55:64), sep="." )


# the data for these sets need to be checked?
p$bc.badlist = c( 
  "NED2012002.17", "TEL2005545.73","NED2015002.7", "NED2015002.8", "NED2015002.9"
) 


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

scanmar.db( DS="scanmar.filtered.redo",  p=p )  # netmind base data filtered for fishing periods .. not really used except for some plots



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




---- 

g = scanmar.db( DS="bottom.contact",  p=p )
plot( I(dist*1.78) ~ I(wing.sa / wing.mean*1000), g)
gr = abs(g$dist*1.78 - (g$wing.sa / g$wing.mean)*1000) 
strange = which ( gr > 1 & g$gear==9 & g$settype==1 )
g[strange, "id"]
 [1] "NED2010027.188" "NED2010027.190" "NED2010027.192" "NED2010027.198" "NED2010027.200" "NED2010027.201"
 [7] "NED2010027.203" "NED2010027.204" "NED2010027.205" "NED2010027.206" "NED2010027.207" "NED2010027.16" 
[13] "NED2010027.17"  "NED2010027.29"  "NED2010027.30"  "NED2010027.31"  "NED2010027.35"  "NED2010027.36" 
[19] "NED2010027.110" "NED2010027.112" "NED2010027.115" "NED2010027.116" "NED2010027.117" "NED2010027.118"
[25] "NED2010027.119" "NED2010027.120" "NED2010027.121" "NED2010027.122" "NED2010027.123" "NED2010027.125"
[31] "NED2010027.126" "NED2010027.127" "NED2010027.128" "NED2010027.129" "NED2010027.133" "NED2010027.134"
[37] "NED2010027.135" "NED2010027.136" "NED2010027.137" "NED2010027.138" "NED2010027.139" "NED2010027.140"
[43] "NED2010027.141" "NED2010027.143" "NED2010027.145" "NED2010027.146" "NED2010027.157" "NED2010027.160"
[49] "NED2010027.162" "NED2010027.165" "NED2010027.166" "NED2010027.167" "NED2010027.168" "NED2010027.169"
[55] "NED2010027.170" "NED2010027.173" "NED2010027.174" "NED2010027.176" "NED2010027.178" "NED2010027.179"
[61] "NED2010027.180" "NED2010027.181" "NED2010027.182" "NED2010027.183" "NED2010027.184" "NED2010027.185"
[67] "NED2010027.186" "NED2010027.187" "NED2010027.208" "NED2010027.209" "NED2010027.210" "NED2010027.211"
[73] "NED2010027.212" "NED2013022.205" "NED2013022.208" "NED2013022.192" "NED2014018.71" 

bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2014018.71") # depth sensor not working
bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2013022.192") # large depth range
bc = scanmar.db( DS="bottom.contact",  p=p , setid= "NED2013022.205") # depth sensor not working
bc = scanmar.db( DS="bottom.contact.redo",  p=p , debugid= "NED2013022.208") # GPS not working
bottom.contact.plot( bc, netspread=TRUE )

