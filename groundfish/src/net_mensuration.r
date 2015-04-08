
p = list()
p$libs = RLibrary("INLA", "numDeriv", "lubridate" )
# this relies upon the gsinf table which is accessible from the groundfish functions
p$inits = loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 
# define location of local data files 
p$scanmar.dir = file.path( project.datadirectory("groundfish"), "data", "nets", "Scanmar" ) 
p$marport.dir = file.path( project.datadirectory("groundfish"), "data", "nets", "Marport" ) 



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
  # tests of extreme data conditions :
 bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2004529.1") # simple, low n 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2004529.18")  # n=300, very curvy data
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2004529.25") # no data
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2005633.40") # simple, low n=308 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2006614.20") # n=191, noisy but simple 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2006614.10") # n=353, noisy 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2007745.74") # n=1876, simple with noisy tail 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2007745.68") # n=1917, large simple curve on bottom 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEM2008830.120") # n= 343, noisy tail 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2009027.70" ) # n=3600 .. flat but sloped bottom
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2009027.91" ) # n=3400 .. .very curvy bottom
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2010027.225" ) # n <100 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2010027.24") # double tow ... should bring up plot  and then continue 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2010027.139" ) # n=1851
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2013028.10") # simple, low n=1663, strange tail 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2013022.175") # ?? 
 bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2005633.111") # ?? 

 bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2005633.107") # ?? 

  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2004529.74")  ###vv strage
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2013022.192")  ###vv strage
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2013022.193")  ###vv strage

}

# the data for these sets need to be checked:
p$bc.badlist = c(
  "TEL2005545.73",  "TEL2005633.41",  "NED2010027.24", 
  "TEL2006614.2",   "TEM2008830.126", 
  "NED2010027.15",  "NED2010027.66", 
  "NED2013028.106", "NED2013028.147", "NED2013028.188", "NED2013028.83", "NED2013028.105", 
  "NED2013022.178", "NED2013022.193", "NED2014102.34",
  "TEL2004529.19","TEL2004529.25","TEL2004529.29","TEL2004529.38","TEL2004529.46","TEL2004529.59", "TEL2004529.78","TEL2004530.23","TEL2004530.55","TEL2004530.60","NED2005001.11","NED2005001.12", "NED2005001.32","NED2005001.43","NED2005001.74","NED2005001.81","NED2005001.82","NED2005001.86", "TEL2005545.11","TEL2005545.12","TEL2005545.32","TEL2005545.43","TEL2005545.54","TEL2005545.9", "TEL2005605.42","TEL2005605.45","TEL2005605.49","TEL2005605.50","TEL2005605.59","TEL2005605.68", "TEL2005605.80","TEL2005633.1",,"TEL2005633.15","TEL2005633.59","TEL2005633.63","NED2006001.47", "NED2006001.80","NED2006030.45","NED2006030.51","NED2006030.72","TEL2006614.1",,"TEL2006614.14", "TEL2006614.3",,"TEL2006614.34","TEL2006614.47","TEL2006614.50","TEL2006614.80","TEL2006614.95", "TEL2007745.18","TEL2007745.20","TEL2007745.38","TEL2007745.43","TEL2007745.73","TEL2007745.96", "TEM2007685.11","TEM2007685.14","TEM2007685.46","TEM2008775.31","TEM2008830.10","TEM2008830.135" "TEM2008830.157","TEM2008830.160","TEM2008830.171","TEM2008830.26","TEM2008830.65","TEM2008830.66", "TEM2008830.93","NED2009027.10","NED2009027.133","NED2009027.135","NED2009027.146","NED2009027.152" "NED2009027.18","NED2009027.26","NED2009027.35","NED2009027.43","NED2009027.49","NED2009027.82", "NED2010001.17","NED2010001.68","NED2010002.28","NED2010002.30","NED2010002.44","NED2010002.48", "NED2010027.125","NED2010027.164","NED2010027.191","NED2010027.21","NED2010027.225","NED2010027.23", "NED2010027.33","NED2010027.41","NED2010027.68","NED2011002.36","NED2011002.58","NED2011002.86", "NED2012002.121","NED2013022.181","NED2013022.24","NED2013028.109","NED2013028.115","NED2013028.119" "NED2013028.127","NED2013028.133","NED2013028.137","NED2013028.142","NED2013028.143","NED2013028.145" "NED2013028.151","NED2013028.153","NED2013028.159","NED2013028.163","NED2013028.165","NED2013028.173" "NED2013028.177","NED2013028.183","NED2013028.185","NED2013028.189","NED2013028.19","NED2013028.21", "NED2013028.25","NED2013028.27","NED2013028.32","NED2013028.42","NED2013028.44","NED2013028.46", "NED2013028.48","NED2013028.5",,"NED2013028.53","NED2013028.67","NED2013028.73","NED2013028.75", "NED2013028.79","NED2013028.87","NED2013028.91","NED2013028.93","NED2013028.95","NED2014002.4", "NED2014002.41","NED2014018.26","NED2014101.36","NED2014101.38","NED2014101.40","NED2014101.48"

)


scanmar.db( DS="bottom.contact.redo",  p=p )  # bring in estimates of bottom contact times from scanmar
scanmar.db( DS="scanmar.filtered.redo",  p=p )  # bring in estimates of bottom contact times from scanmar
scanmar.db( DS="sweptarea.redo",  p=p )  



# --- stats /analysis

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


