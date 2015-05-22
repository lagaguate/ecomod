
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


if (FALSE) { 
  # tests of extreme data conditions :
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2004529.1") # simple, low n 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2010027.139" ) # n=1851
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2013022.192")  ### large depth range and wrong
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2013028.10") # simple, n=1663, strange tail 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2014102.35") # constant depth data ...  
  bottom.contact.plot( bc, netspread=TRUE )
}


# the data for these sets need to be checked?
p$bc.badlist = c( 
  "NED2012002.17", "TEL2005545.73","NED2015002.7", "NED2015002.8", "NED2015002.9"
) 

scanmar.db( DS="bottom.contact.redo",  p=p )  # bring in estimates of bottom contact times from scanmar
scanmar.db( DS="scanmar.filtered.redo",  p=p )  # bring in estimates of bottom contact times from scanmar
scanmar.db( DS="sweptarea.redo",  p=p )  


create.marport.database = FALSE
if (create.marport.database ) {
  p$netmensuration.years = 2013:2014  
  marport.db( DS="basedata.redo",  p=p )      # load data from raw data files
  marport.db( DS="gated.redo",  p=p )      # QA/QC of data
  marport = marport.db( DS="gated",  p=p )
}



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


