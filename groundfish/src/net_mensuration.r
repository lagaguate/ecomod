
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
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2009027.155") # simple, low n 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2004529.1") # simple, low n 

  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2004529.18")  # n=300, very curvy data

  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2004529.25") # no data

  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2004529.74")  ### echos noisy
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2005633.111") # ?? 

  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2005633.107") # deep 


  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2005633.40") # simple, low n=308 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2006614.20") # n=191, noisy but simple 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2006614.10") # n=353, noisy 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2007745.74") # n=1876, simple with noisy tail 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2007745.68") # n=1917, large simple curve on bottom 
 

### not working
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEM2008830.120") # n= 343, noisy tail 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "TEL2004529.4") # n= 343, noisy tail 
  
  
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2009027.70" ) # n=3600 .. flat but sloped bottom
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2009027.91" ) # n=3400 .. .very curvy bottom
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2010027.225" ) # n <100 

  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2010027.24") # double tow ... should bring up plot  and then continue 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2010027.178") # 
  
  
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2010027.139" ) # n=1851
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2013022.192")  ### large depth range and wrong
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2013022.193")  ### flat line
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2013022.147")  ### ... noise
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2013022.12")  ###vv strage
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2013022.14")  ###vv strage
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2013022.181")  ###vv strage
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2013022.175") # ?? 
  bc = scanmar.db( DS="bottom.contact.redo",  p=p , bottom.contact.debug.id= "NED2013028.10") # simple, n=1663, strange tail 

}


# the data for these sets need to be checked:
p$bc.badlist = c(
  "tel2005545.73",  "tel2005633.41",  "ned2010027.24", 
  "tel2006614.2",   "tem2008830.126", 
  "ned2010027.15",  "ned2010027.66",  "tel2005633.63", 
  "ned2013028.106", "ned2013028.147", "ned2013028.188", "ned2013028.83", "ned2013028.105", 
  "ned2013022.178", "ned2013022.193", "ned2014102.34",  "ned2013028.104", "ned2013028.148",
  "tel2004529.19" , "tel2004529.25",  "ned2013028.168", "ned2013028.174", "tel2004529.46",  "tel2004529.59" ,
 "tel2004529.78" , "tel2004530.23",  "ned2013028.59",  "tel2004530.55",  "ned2005001.11",  "ned2005001.12" ,
"ned2005001.32",  "ned2013028.9", "ned2012002.66",  "ned2005001.74",  "ned2005001.81" , "ned2005001.82",  "ned2005001.86",  "ned2005001.91" ,
  "ned2009027.135", "ned2012002.81" , "tel2005545.11" , "tel2005545.12" , "tel2005545.21",  "ned2009027.146",
 "ned2012002.92",  "ned2009027.152", "ned2009027.154", "tel2005545.9" ,  "tel2005605.1" ,  "tel2005605.42" ,
 "tel2005605.45" , "tel2005605.50",  "tel2005605.59" , "tel2005605.68",  "tel2005605.80" , "tel2005633.1"  ,
 "tel2005633.106", "ned2009027.35"  ,"tel2005633.15" , "ned2009027.43" , "tel2005633.43" , "tel2005633.50" ,
 "tel2005633.59",  "ned2006001.47" , "ned2006001.59" , "ned2006001.80" ,
 "ned2012002.92"  ,"ned2009027.152", "ned2009027.154", "ned2014018.28",  
 "ned2014002.4",   "ned2014018.13" , "ned2014018.168", "ned2014018.169", "ned2014018.214" ,"ned2014018.225",
 "ned2014018.33" , "ned2014101.14"
  )




scanmar.db( DS ="bottom.contact.redo",  p=p )  # bring in estimates of bottom contact times from scanmar
scanmar.db( DS="scanmar.filtered.redo",  p=p )  # bring in estimates of bottom contact times from scanmar
scanmar.db( DS="sweptarea.redo",  p=p )  



# --- stats /analysis

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


create.marport.database = FALSE
if (create.marport.database ) {
  p$netmensuration.years = 2013:2014  
  marport.db( DS="basedata.redo",  p=p )      # load data from raw data files
  marport.db( DS="gated.redo",  p=p )      # QA/QC of data
  marport = marport.db( DS="gated",  p=p )
}


