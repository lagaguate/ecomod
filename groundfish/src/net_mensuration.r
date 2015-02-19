
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
  scanmar.db( DS="scanmar.filtered.redo",  p=p )  # bring in estimates of bottom contact times from scanmar
  scanmar.db( DS="sweptarea.redo",  p=p )  



create.marport.database = FALSE
if (create.marport.database ) {
  marport.db( DS="marport.redo",  p=p )      # load data from raw data files
  marport.db( DS="marport.gated.redo",  p=p )      # QA/QC of data
  marport = marport.db( DS="marport.gated",  p=p )
}


