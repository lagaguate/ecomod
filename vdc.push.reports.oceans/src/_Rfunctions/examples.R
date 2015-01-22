# Environment and Running a Default Report ------------------------

  #Before you can run a report, you must load in the required functions
  #These all exist in ecomod, and are loaded via the command
  
    #loadfunctions("vdc.push.reports.oceans")

  #For maximum convenience, set your credentials in your .Rprofile:
   
    #oracle.dsn = "PTRAN"
    #oracle.oceans.user = <your username>
    #oracle.oceans.pw   = <your password>

  #If your credentials are set, you can run the default report via:

    #oceans.activity.mapper()

  #Otherwise you have to include them in the function call like this:

    #oceans.activity.mapper("PTRAN","<your username>","<your password>")

  #The default report getting the last 30 days of data for 
  #Lophelia, NE Channel, Gully, Vazella, ST Anns and Musquash.
  
# Custom Reports --------------------------------------------------

  #Instead of simply running reports for the last 30 days, you can 
  #also run reports for specific date ranges or specific vessels.
  #You may also choose to run a report for a particular area
  #rather than for all of them

  #Following are the default values that are used if none are specified

  # dsn         = oracle.dsn                 #The name of your odbc connection to oracle
  # user        = oracle.oceans.user         #Your oracle user name
  # pw          = oracle.oceans.password     #Your oracle password
  # debug       = F                          #debug - generally leave it as F
  # last_n_days = 30                       #previous x days (eg 30)                         
  # startdate   = NULL                       #Local startdate (eg '2013-10-27 16:00')
  # enddate     = NULL                       #Local enddate (eg '2013-11-27 05:00')
  # vessel_list = c()                        #vector of vrn (eg c(999999,888888) ) - NO quotes!
  # datawindows = c("Lophelia CCA",          #vector of possible values (see list below) - 
  #                 "Northeast Channel",     #case sensitive, must use quotes!
  #                 "Gully",
  #                 "VazellaEmerald",
  #                 "St Anns Bank Inventory Box",
  #                 "Musquash")

# Available datawindows -------------------------------------------

  # default datawindows
    # Lophelia CCA
    # Northeast Channel
    # Gully
    # VazellaEmerald
    # St Anns Bank Inventory Box
    # Musquash

  # other datawindows
    # Russian Hat Assessment Box1
    # Stone Fence
    # Haddock Closed Area
    # roseway whale
    # Fundy Whale Sanctuary
    # All                #All gets one massive file of all areas - 
                         #just like the old (wrong) NE channel report

  #"Haldimand Canyon" and "Shortland Canyon" don't have 
  #data windows so they can't be generated.

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
      #oceans.activity.mapper()
    #minimalist (with credentials)
      #oceans.activity.mapper("PTRAN","<your username>","<your password>")
    #full function call
      #oceans.activity.mapper(dsn="PTRAN", user="<your username>", pw="<your password>",debug=F,last_n_days=,startdate=NULL, enddate=NULL, vessel_list=c(),datawindows=c("Lophelia CCA","Northeast Channel","Gully","VazellaEmerald","St Anns Bank Inventory Box","Musquash"))
  
  ##examples for calling the vdc.oceans.activity.mapper code:
    ##last 15 days in the Gully
      #oceans.activity.mapper(dsn=oracle.oceans.dsn, user=oracle.oceans.user, pw=oracle.oceans.pw, debug=F,last_n_days=15,startdate=NULL, enddate=NULL, ,datawindows=c("Gully"))
    
    ##activity for 2014 of 2 particular vessels in all areas
      #oceans.activity.mapper(dsn=oracle.oceans.dsn, user=oracle.oceans.user, pw=oracle.oceans.pw, debug=F,last_n_days=NULL,startdate="2014-01-01 00:01", enddate="2014-12-31 23:59", vessel_list=c(888888, 999999),datawindows=c("All"))
    
    ##activity for june for a single vessel in multiple areas
      #oceans.activity.mapper(dsn=oracle.oceans.dsn, user=oracle.oceans.user, pw=oracle.oceans.pw, debug=F,last_n_days=NULL,startdate="2014-06-01 00:01", enddate="2014-06-31 23:59", vessel_list=c(777777),datawindows=c("Lophelia CCA","Northeast Channel","Gully","VazellaEmerald","St Anns Bank Inventory Box","Musquash"))
    
    ##get the activity for the 60 days prior to Mike's birthday for Lophelia
      #oceans.activity.mapper(dsn=oracle.oceans.dsn, user=oracle.oceans.user, pw=oracle.oceans.pw, debug=F,last_n_days=60,startdate=NULL, enddate="2014-02-11 12:00", vessel_list=c(),datawindows=c("Lophelia CCA"))