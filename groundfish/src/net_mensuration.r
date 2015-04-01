
p = list()
p$libs = RLibrary("INLA", "numDeriv", "lubridate" )
# this relies upon the gsinf table which is accessible from the groundfish functions
p$inits = loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 
# define location of local data files 
p$scanmar.dir = file.path( project.directory("groundfish"), "data", "nets", "Scanmar" ) 
p$marport.dir = file.path( project.directory("groundfish"), "data", "nets", "Marport" ) 



# pick the year to process:
# p$netmensuration.years = p$current.year  ## for incremental/annual update
## 2009 is the first year with set logs from scanmar available .. if more are found, alter this date
p$current.year = 2015
p$netmensuration.years = c(1990:1992, 2004:p$current.year)  
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


# the following works upon annual time slices ( defined in p$netmensuration.years )
scanmar.db( DS="basedata.redo", p=p )        # Assimilate Scanmar files in raw data saves *.set.log files
scanmar.db( DS="basedata.lookuptable.redo", p=p ) # match modern data to GSINF positions and extract Mission/trip/set ,etc
scanmar.db( DS="sanity.checks.redo",  p=p )      # QA/QC of data

# WARNING:: the following may crash as INLA does not exit gracefully from some errors
# and R cannot catch the faults .. restart R or reboot the system (it can happen) 
# and then re-run the line and it will continue from where it crashed ... update the bc.badlist too
# doing it year by year is probably wise for now
# usually insufficient data for these or just flat-lines .. no reliable data


if (FALSE) {
  # tests
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2007745.74") # simple with noisy tail 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2007745.68") # large simple curve on bottom 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2010027.24") # double tow ... should bring up plot  and then continue 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2004529.1") # simple, low n 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2013028.10") # simple, low n 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2013022.175") # simple, low n 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2005633.40") # simple, low n 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2006614.20") 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2006614.10") 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEM2008830.120") 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2004529.18")
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2004529.25")
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2009027.70" )
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2010027.225" )

}

# the data for these sets need to be checked:
p$bc.badlist = c(
  "TEL2005545.73",  "TEL2005633.41",  "NED2010027.24", 
  "TEL2006614.2",   "TEM2008830.126", 
  "NED2010027.15",  "NED2010027.66", 
  "NED2013028.106", "NED2013028.147", "NED2013028.188", "NED2013028.83", "NED2013028.105", 
  "NED2013022.178"
)

#  "NED2010027.24" is a double tow

scanmar.db( DS="bottom.contact.redo",  p=p )  # bring in estimates of bottom contact times from scanmar

scanmar.db( DS="scanmar.filtered.redo",  p=p )  # bring in estimates of bottom contact times from scanmar

scanmar.db( DS="sweptarea.redo",  p=p )  

gs = scanmar.db( DS="bottom.contact",  p=p )  # bring in estimates of bottom contact times from scanmar
pp = tapply( gs$id, year(gs$bc0.datetime), function(x) { length(unique(x))} )
pp = data.frame( yr= rownames(pp), n.bc=pp)
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


create.marport.database = FALSE
if (create.marport.database ) {
  p$netmensuration.years = 2013:2014  
  marport.db( DS="basedata.redo",  p=p )      # load data from raw data files
  marport.db( DS="gated.redo",  p=p )      # QA/QC of data
  marport = marport.db( DS="gated",  p=p )
}


