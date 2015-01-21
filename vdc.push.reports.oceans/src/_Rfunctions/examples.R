#oceans.activity.mapper(debug,last_n_days,startdate,enddate,vessel_list,datawindows)

#Altering report parameters

#NOTE regarding date specifity:
#if neither start or end date is entered, data is returned relative to NOW
#if both dates are entered, data is returned for that range
#if one date is entered, data is returned for x days prior to that date

#debug       = T                                      #"T" reuses last data-grab, "F" gets new data
#last_n_days = 30                                     #previous x days (eg 30)
#startdate   = NULL                                   #Local startdate (eg '2013-10-27 16:00')
#enddate     = NULL                                   #Local enddate (eg '2013-11-27 05:00')  
#vessel_list = c()                                    #vector of vrn (eg c(999999,888888) ) - NO quotes!
#datawindows = c("Lophelia CCA","Northeast Channel")  #vector of possible values (see list below) - case sensitive, must use quotes!
  # Lophelia CCA
  # Northeast Channel
  # Gully
  # VazellaEmerald
  # St Anns Bank Inventory Box
  # Musquash  

  ####others
  #Russian Hat Assessment Box1
  #Stone Fence
  #Haddock Closed Area
  #roseway whale
  #Fundy Whale Sanctuary
  #All                #All gets one massive file of all areas - just like the old (wrong) NE channel report

# #examples for calling the vdc.oceans.activity.mapper code:
#   #get the last 15 days in the Gully
#   oceans.activity.mapper(debug=F,last_n_days=15,startdate=NULL, enddate=NULL, ,datawindows=c("Gully"))
#   #get the activity for 2014 of a particular vessel in all areas
#   oceans.activity.mapper(debug=F,last_n_days=NULL,startdate="2014-12-31 23:59", enddate="2014-01-01 00:01", vessel_list=c(999999),datawindows=c("All"))
#   #same as above, but only for particular areas
#   oceans.activity.mapper(debug=F,last_n_days=NULL,startdate="2014-12-31 23:59", enddate="2014-01-01 00:01", vessel_list=c(999999),datawindows=c("Lophelia CCA","Northeast Channel","Gully","VazellaEmerald","St Anns Bank Inventory Box","Musquash"))
#   #get the activity for the 60 days prior to Mike's birthday for Lophelia
#   oceans.activity.mapper(debug=F,last_n_days=60,startdate="2014-02-11 00:01", enddate=NULL, vessel_list=c(),datawindows=c("Lophelia CCA"))
  
 