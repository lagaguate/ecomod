
p = list()
p$libs = RLibrary("INLA", "numDeriv", "lubridate" )
# this relies upon the gsinf table which is accessible from the groundfish functions
p$inits = loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 
# define location of local data files 
p$scanmar.dir = file.path( project.directory("groundfish"), "data", "nets", "Scanmar" ) 
p$marport.dir = file.path( project.directory("groundfish"), "data", "nets", "Marport" ) 
p$current.year = 2014
# pick the year to process:
# p$netmensuration.years = p$current.year  ## for incremental/annual update
## 2009 is the first year with set logs from scanmar available .. if more are found, alter this date
p$netmensuration.years = c(1990:1992, 2004:p$current.year)  



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
p$netmensuration.years = c(1990:1992, 2004:p$current.year)  
scanmar.db( DS="basedata.redo", p=p )        # Assimilate Scanmar files in raw data saves *.set.log files
scanmar.db( DS="basedata.lookuptable.redo", p=p ) # match modern data to GSINF positions and extract Mission/trip/set ,etc
scanmar.db( DS="sanity.checks.redo",  p=p )      # QA/QC of data

# WARNING:: the following may crash as INLA does not exit gracefully from some errors
# and R cannot catch the faults .. restart R or reboot the system (it can happen) 
# and then re-run the line and it will continue from where it crashed ... update the bc.badlist too
# doing it year by year is probably wise for now
# usually insufficient data for these or just flat-lines .. no reliable data
# but .. NED2010027.66 looks to be two tows in one file ... need to break file manually ...
#  "NED2010027.24"

p$bc.badlist = c(
  "NED2013028.106", "NED2013028.147", "NED2013028.188", "NED2013028.83", "NED2010027.15", 
  "NED2010027.29", "NED2010027.66", "NED2010027.8", "NED2013028.105", "NED2013022.178", 
   "TEL2005545.73",  "TEL2005633.41",  "TEL2006614.2",   "TEM2008830.126",  "NED2010027.24" 
)

scanmar.db( DS="bottom.contact.redo",  p=p )  # bring in estimates of bottom contact times from scanmar

scanmar.db( DS="scanmar.filtered.redo",  p=p )  # bring in estimates of bottom contact times from scanmar
scanmar.db( DS="sweptarea.redo",  p=p )  


nm = scanmar.db( DS="bottom.contact",  p=p )  # bring in estimates of bottom contact times from scanmar
nm = scanmar.db( DS="scanmar.filtered",  p=p )  # bring in estimates of bottom contact times from scanmar


create.marport.database = FALSE
if (create.marport.database ) {
  p$netmensuration.years = 2013:2014  
  marport.db( DS="basedata.redo",  p=p )      # load data from raw data files
  marport.db( DS="gated.redo",  p=p )      # QA/QC of data
  marport = marport.db( DS="gated",  p=p )
}


