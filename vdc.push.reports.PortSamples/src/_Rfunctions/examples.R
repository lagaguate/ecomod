# Environment and Running a Default Report ------------------------

#Before you can run a report, you must load in the required functions
#These all exist in ecomod, and are loaded via the command

#loadfunctions("vdc.push.reports.PortSamples")

#For maximum convenience, set your credentials in your .Rprofile:

#oracle.dsn = "PTRAN"
#oracle.PortSamples.user = <your username>
#oracle.PortSamples.password   = <your password>

#If your credentials are set, you can run the default report via:

#PortSamples.activity.mapper()

#Otherwise you have to include them in the function call like this:

#oceans.activity.mapper("PTRAN","<your username>","<your password>")

#The default report gets the last 5 days of data.

# Custom Reports --------------------------------------------------

#Instead of simply running reports for the last 5 days, you can 
#also run reports for specific date ranges or specific vessels.

#Following are the default values that are used if none are specified

# dsn         = oracle.dsn                      #The name of your odbc connection to oracle
# user        = oracle.PortSamples.user         #Your oracle user name
# pw          = oracle.PortSamples.password     #Your oracle password
# debug       = F                               #debug - generally leave it as F
# last_n_days = 5                               #previous x days (eg 5)                         
# startdate   = NULL                            #Local startdate (eg '2013-10-27 16:00')
# enddate     = NULL                            #Local enddate (eg '2013-11-27 05:00')
# vessel_list = c()                             #vector of vrn (eg c(999999,888888) ) - NO quotes!

# Date Specificity ------------------------------------------------

#if both dates are entered (start and end), data is returned for that
#range

#if a single date (start or end) is entered, the last_n_days of data
#are returned prior to that date 

#if no dates are entered, the last_n_days are relative to the time 
#when you ran the report

#if no value is given for last_n_days, a value of 30 is used

# Examples --------------------------------------------------------

#Default reports
#minimalist, using all defaults
#PortSamples.activity.mapper()
#minimalist (with credentials)
#PortSamples.activity.mapper("PTRAN","<your username>","<your password>")
#full function call
#PortSamples.activity.mapper(dsn="PTRAN", user="<your username>", pw="<your password>",debug=F,last_n_days=,startdate=NULL, enddate=NULL, vessel_list=c())

##examples for calling the vdc.oceans.activity.mapper code:
##last 100 days
#PortSamples.activity.mapper(dsn=oracle.dsn, user=oracle.PortSamples.user, pw=oracle.PortSamples.password, debug=F,last_n_days=100,startdate=NULL, enddate=NULL,vessel_list=c())

##activity for 2014 of 2 particular vessels
#PortSamples.activity.mapper(dsn=oracle.dsn, user=oracle.PortSamples.user, pw=oracle.PortSamples.password, debug=F,last_n_days=NULL,startdate="2014-01-01 00:01", enddate="2014-12-31 23:59", vessel_list=c(888888, 999999))

##activity for june for a single vessel
#PortSamples.activity.mapper(dsn=oracle.dsn, user=oracle.PortSamples.user, pw=oracle.PortSamples.password, debug=F,last_n_days=NULL,startdate="2014-06-01 00:01", enddate="2014-06-31 23:59", vessel_list=c(777777))

##get the activity for the 60 days prior to Mike's birthday
#PortSamples.activity.mapper(dsn=oracle.dsn, user=oracle.PortSamples.user, pw=oracle.PortSamples.password, debug=F,last_n_days=60,startdate=NULL, enddate="2014-02-11 12:00", vessel_list=c())